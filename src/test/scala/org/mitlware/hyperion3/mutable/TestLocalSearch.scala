package org.mitlware.hyperion3.mutable

import org.mitlware._
import org.mitlware.mutable._

import hyperion3.mutable._

import hyperion3.benchmarks.onemax.mutable._

import statelet.bitvector._

import java.lang.{Double=>JDouble}

import org.junit._
import org.junit.Assert._

import scala.util.Random

//////////////////////////////////////////////////////////////////////

final class TestOnemaxLocalSearch {

  val RandomSeed = 0x12345678L
  val random = new Random( RandomSeed )
  
  @Before
  def setUp() : Unit = {
    random.setSeed(RandomSeed)
  }

  /////////////////////////////////
  
  def testLSReturnBestImpl[S](initialState: S, 
    maxIter: Int,
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long],    
    perturb: Perturb[S],      
    accept: Accept[S],
    prefer: Prefer[S], 
    eval: Evaluate[S,JDouble],
    isFinished: IsFinished[S]) : S = {
    
    val search : IteratedPerturb[S] = LocalSearch.returnBest( 
        perturb, accept, isFinished, prefer, iterCount, startTime )    

    search( initialState )
  }

  /////////////////////////////////
  
  def testSAImpl[S](initialState: S, 
    maxIter: Int,
    initialTemperature: Double, 
    random: ReadWrite[Random],    
    perturb: Perturb[S],      
    prefer: Prefer[S], 
    eval: Evaluate.Directional[S,JDouble], 
    isFinished: IsFinished[S],
    mkVar: VariableFactory ) : S = {

    val iterCount = mkVar.readWrite(0L)
    val startTime = mkVar.readWrite(0L)
    val temperature = mkVar.readWrite(initialTemperature) 
    
    def schedule( temp: Double, iter: Long ) : Double = 
      jeep.math.LinearInterpolation.apply( iter, 0, maxIter, initialTemperature, 0.0 )
    
    val search : IteratedPerturb[S] = LocalSearch.simulatedAnnealing( perturb, 
      isFinished, prefer, eval, schedule, iterCount, startTime, temperature, random )    

    search( initialState )
  }
  
  /////////////////////////////////
  
  import org.apache.commons.collections15.buffer.CircularFifoBuffer
  
  def testTSImpl[S,Feature](initialState: S, 
    iterCount: ReadWrite[Long],
    startTime: ReadWrite[Long],    
    tabuList: ReadWrite[CircularFifoBuffer[Set[Feature]]],
    featureFn: ( S, S ) => Set[Feature], 
    perturb: Perturb[S],      
    prefer: Prefer[S], 
    isFinished: IsFinished[S],
    eval: Evaluate.Directional[S,JDouble] ) : S = {
    
    val search : IteratedPerturb[S] = LocalSearch.strictTabu( perturb, 
      isFinished, prefer, iterCount, startTime, featureFn, tabuList )    

    search( initialState )
  }
  
  /////////////////////////////////  
  
  def testUniformMutationImpl(mkVar: VariableFactory) : BitVector = {

    import hyperion3.benchmarks.onemax.mutable._

    val eval = Onemax.countOnes
    val prefer = eval
    val accept = Accept.improving( prefer )
    val perturb = Onemax.uniformMutation( mkVar.readWrite(random) )
    
    val iterCount = mkVar.readWrite(0L)
    val startTime = mkVar.readWrite(0L)

    val NumBits = 32
    val initialState = new BitVector( NumBits )

    val MaxIter = 8*NumBits
    
    import hyperion3.mutable.isfinished._    
    val maxIterReached = maxIterations[BitVector](MaxIter,iterCount) _
    def optimumFound(x: BitVector) = eval(x) == 1.0
    def isFinished(x: BitVector) = maxIterReached(x) || optimumFound(x)  
    
    testLSReturnBestImpl(initialState, MaxIter,iterCount, startTime, perturb, accept, prefer, eval, isFinished _ )
  }

  @Test  
  def testUniformMutation() : Unit = {
    {
      val globalFactory = hyperion3.mutable.GlobalVars.Factory
      val result = testUniformMutationImpl( globalFactory )
      assertEquals( 1.0, Onemax.countOnes( result ), 0.0 )      
    }
    
    {
      implicit val workspace = Workspace()
      val workspaceFactory = new hyperion3.mutable.WorkspaceVars.Factory
      val result = testUniformMutationImpl( workspaceFactory )
      assertEquals( 1.0, Onemax.countOnes( result ), 0.0 )      
    }
  }
  
  /////////////////////////////////
  
  def testBitFlipLocalityImpl(mkVar: VariableFactory) : BitVector = {

    val eval = Onemax.countOnes
    val prefer = eval
    val accept = Accept.improving( prefer )
    val locality : Locality[BitVector] = Onemax.bitflipLocality _
      
    val perturb  = Perturb.compose( locality, Move.best( eval ) )
    
    val iterCount = mkVar.readWrite(0L)
    val startTime = mkVar.readWrite(0L)

    val NumBits = 32
    val MaxIter = NumBits
    
    import hyperion3.mutable.isfinished._    
    val maxIterReached = maxIterations[BitVector](MaxIter,iterCount) _
    def optimumFound(x: BitVector) = eval(x) == 1.0
    def isFinished(x: BitVector) = maxIterReached(x) || optimumFound(x)  
    
    val initialState = new BitVector( NumBits )      
    testLSReturnBestImpl(initialState,maxIter=MaxIter,iterCount,startTime,perturb, accept, prefer, eval,isFinished _ )
  }
  
  /////////////////////////////////
  
  @Test
  def testBitFlipLocality() : Unit = {
    
    {
      val globalFactory = hyperion3.mutable.GlobalVars.Factory
      val result = testBitFlipLocalityImpl( globalFactory )
      assertEquals( 1.0, Onemax.countOnes( result ), 0.0 )      
    }
    
    {
      implicit val workspace = Workspace()
      val workspaceFactory = new hyperion3.mutable.WorkspaceVars.Factory
      val result = testBitFlipLocalityImpl( workspaceFactory )    
      //println(workspace)
      
      assertEquals( 1.0, Onemax.countOnes( result ), 0.0 )
    }
  }
  
  /////////////////////////////////

  @Test
  def testSA() : Unit = {

    val varFactory = hyperion3.mutable.GlobalVars.Factory    
    
    /////////////////////////////
    
    val eval = Onemax.countOnes
    val prefer = eval
    val accept = Accept.improving( prefer )
    val perturb = Onemax.uniformMutation( varFactory.readWrite(random) )     

    val NumBits = 8
    val initialState = new BitVector( NumBits )

//    val iterCount = hyperion3.mutable.GlobalVars.Mutable(0L)
//    val startTime = hyperion3.mutable.GlobalVars.Mutable(0L)    
//    val temperature = hyperion3.mutable.GlobalVars.Mutable(100.0)    
    
    val InitialTemperature = 100.0 
    val MaxIter = 16 * NumBits

    import hyperion3.mutable.isfinished._    
    // val maxIterReached = maxIterations[BitVector](MaxIter,iterCount) _
    def optimumFound(x: BitVector) : Boolean = eval(x) == 1.0
    def isFinished(x: BitVector) = optimumFound(x) // || maxIterReached(x)   
    
    val globalFactory = hyperion3.mutable.GlobalVars.Factory    
    
    val result = testSAImpl(initialState, maxIter=MaxIter, InitialTemperature, 
      globalFactory.readWrite(random), perturb, prefer, eval, isFinished _, varFactory )
    assertEquals( 1.0, eval( result ), 0.0 )      
  }
  
  /////////////////////////////////

  @Test
  def testTS() : Unit = {

    val eval = Onemax.countOnes
    val prefer = eval
    val accept = Accept.improving( prefer )

    val varFactory = hyperion3.mutable.GlobalVars.Factory    
    
    val lastPerturbedIndices = varFactory.readWrite(Set.empty[Int])
    val perturb = Onemax.uniformMutationInstrumented( lastPerturbedIndices, varFactory.readWrite(random) ) _
    // val perturb = uniformMutation( hyperion3.mutable.GlobalVars.Mutable(random) ) _      

    val NumBits = 8
    val initialState = new BitVector( NumBits )
    
    val iterCount = varFactory.readWrite(0L)
    val startTime = varFactory.readWrite(0L)    
    
    type Feature = Int // bit index
    val TabuListLength = 7
    
    val tabuList = varFactory.readWrite( new CircularFifoBuffer[Set[Feature]](TabuListLength) )

    def featureFn( incoming: BitVector, incumbent: BitVector ) : Set[Feature] = {
      lastPerturbedIndices.get
    } 
    
    val MaxIter = 4 * NumBits

    import hyperion3.mutable.isfinished._    
    val maxIterReached = maxIterations[BitVector](MaxIter,iterCount) _
    def optimumFound(x: BitVector) = eval(x) == 1.0
    def isFinished(x: BitVector) = maxIterReached(x) || optimumFound(x)  
    
    val result = testTSImpl(initialState, iterCount, 
        startTime, tabuList, 
        featureFn, perturb, prefer, isFinished _, eval )
    assertEquals( 1.0, eval( result ), 0.0 )        
  }
}

// End ///////////////////////////////////////////////////////////////

