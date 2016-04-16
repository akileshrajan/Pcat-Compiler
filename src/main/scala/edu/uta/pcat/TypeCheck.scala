/****************************************************************************************************
 *
 * File: TypeCheck.scala
 * The type-checker for PCAT programs
 *
 ****************************************************************************************************/

package edu.uta.pcat

import scala.collection.immutable.ListMap


abstract class TypeChecker {
  var trace_typecheck = false

  /** symbol table to store PCAT declarations */
  var st = new SymbolTable

  /* various types */
  val anyType    = AnyType()
  val noType     = NamedType("NoType")
  val intType    = NamedType("INTEGER")
  val boolType   = NamedType("BOOLEAN")
  val floatType  = NamedType("FLOAT")
  val stringType = NamedType("STRING")

  def expandType ( tp: Type ): Type
  def typecheck ( e: Expr ): Type
  def typecheck ( e: Lvalue ): Type
  def typecheck ( e: Stmt )
  def typecheck ( e: Body, returned: Type )
  def typecheck ( e: ProcDecl )
}


class TypeCheck extends TypeChecker {

  /** the expected type of the return value from the current function */
  var expected_returned_type: Type = null

  /** If tp is a named type, expand it */
  def expandType ( tp: Type ): Type = {
    if (tp.equals(intType) || tp.equals(boolType)
        || tp.equals(floatType) || tp.equals(stringType)
        || tp.equals(anyType) || tp.equals(noType))
      tp
    else tp match {
      case NamedType(nm)
        => st.lookup(nm) match {
          case Some(TypeDec(t))
              => expandType(t)
          case _ => throw new Error("Undeclared type: "+tp)
        }
      case _ => tp
    }
  }

  /** returns true if the types tp1 and tp2 are equal under name equivalence */
  def typeEquivalence ( tp1: Type, tp2: Type ): Boolean = {
    /* AnyType() matches any RecordType(...) */
	
    if (tp1.equals(tp2))
      true
    else expandType(tp1) match {
      case RecordType(_)
        => tp2.equals(anyType)
      case _ => expandType(tp2) match {
        case RecordType(_)
            => tp1.equals(anyType)
        case _ => false
      }
    }
  }

  /* Tracing level */
  var level: Int = -1

  def trace[T] ( e: Any, result: => T ): T = {
    if (trace_typecheck) {
      level += 1
      println(" "*(3*level)+"** "+e)
    }
    val res = result
    if (trace_typecheck) {
      print(" "*(3*level))
      if (res == ())
        println("->")
      else println("-> "+res)
      level -= 1
    }
    res
  }

  /** typecheck an expression AST */
  
  def typecheck ( e: Expr ): Type =
    trace(e,e match {
      case BinOpExp(op,l,r) => {
        val ltp = typecheck(l)
        val rtp = typecheck(r)
        if (!typeEquivalence(ltp,rtp))
          throw new Error("Incompatible types in binary operation: "+e)
        else if (op.equals("and") || op.equals("or"))
               if (typeEquivalence(ltp,boolType))
                 ltp
               else throw new Error("AND/OR operation can only be applied to booleans: "+e)
               else if (op.equals("eq") || op.equals("neq"))
                      boolType
               else if (!typeEquivalence(ltp,intType) && !typeEquivalence(ltp,floatType))
                      throw new Error("Binary arithmetic operations can only be applied to integer or real numbers: "+e)
               else if (op.equals("gt") || op.equals("lt") || op.equals("geq") || op.equals("leq"))
                      boolType
               else ltp
    }
	   /* PUT YOUR CODE HERE */
	   
	   
	case UnOpExp( op, r ) => {	
		val rtp = typecheck(r)
		if (op.equals("minus") && typeEquivalence(rtp,intType) )
			intType
		else if(op.equals("minus") && typeEquivalence(rtp,floatType))	
			floatType
		else if(op.equals("not") && typeEquivalence(rtp,boolType) )
			boolType
		else throw new Error("Wrong unary operation: "+e)					
	}
	
case LvalExp(l) => {
		typecheck(l)				
	}
	
	case CallExp ( n, arg) 
	=> {st.lookup(n) match{
			case Some(ProcDec(otp,params,_,_,_))	=>{			
			if(params.length != arg.length )
			throw new Error("Number of parameter doesn't match")
			else (arg.map(typecheck(_)).zip(params)).map({
					case (atp:Type,(_,ptp:Type))
					=>if(!typeEquivalence(atp,ptp))
						throw new Error("The type of call argument ("+atp+") does not match the type of the formal parameter: "+ptp)
			})	
			otp
			
			}
			case _ => throw new Error("Undefined function: "+n) 

		}
	} 

	case IntConst( v ) => {intType}
	
	case RealConst( v ) => {floatType}
	
	case StringConst( v ) => {stringType}	
	
	
	case ArrayExp( nm, arg3 ) 
	=> {
		
		st.lookup(nm) match{
			case Some(TypeDec(ArrayType(t))) =>{ val at = NamedType(t)
										
						arg3.map{case(x,y) =>if(typeEquivalence(intType,typecheck(x)))
													intType
											if(typeEquivalence( at,typecheck(y)))		
													at					 
											else throw new Error("Wrong Array Experssion: "+x+"and"+y)  
								}	
				NamedType(nm)						
										
			 } 
			case Some(_) => throw new Error(nm+" is not a Array")
			case None => throw new Error("Undefined Array: "+nm)
		}	
		
		
	}
	
	case RecordExp ( nm, arg)	
	=> {
				
				
		st.lookup(nm) match{
						case Some(TypeDec(RecordType(t))) =>{ 
															if(arg.length!=t.length)
															{throw new Error("Wrong number of attributes: "+arg)}
															
															
															
															(arg.zip(t)).map{															
															
																		case((x,y),(u,v)) =>
																					val rt=NamedType(v)
																					if(x.equals(u)){
																							if(typeEquivalence(rt,typecheck(y)))
																								rt
																							 
																					
																					}else throw new Error("Wrong Record attribute: "+x) 
																		
																			}
																
								
															NamedType(nm)		
														}	
							case Some(_) => throw new Error(nm+" is not a Array")
							case None => throw new Error("Undefined : "+nm)
						
						}				
				
		}
	
				
	
	
     

      case _ => throw new Error("Wrong expression(1): "+e)
    } )
	

	
  /** typecheck an Lvalue AST ******/
  
  def typecheck ( e: Lvalue ): Type =
    trace(e,e match {
		
		
		case	Var("TRUE")
		=>{ boolType}
						
						
		case	Var("FALSE")
		=>{	boolType}
						
						
		case	Var("NIL")
		=>{ anyType	}  

		
      case Var(name)
        => st.lookup(name) match {
              case Some(VarDec(t,_,_)) => expandType(t)
              case Some(_) => throw new Error(name+" is not a variable")
              case None => throw new Error("Undefined variable: "+name)
      }

      /* PUT YOUR CODE HERE   */             
	
	case ArrayDeref( arr, indx )
        =>{
			
			val indexp = typecheck(indx)
			val ltp = typecheck(arr)
			if(typeEquivalence(intType,indexp))
				intType
			else throw new Error("Incorrect Index "+indx)	
	  }
		
	case RecordDeref ( rec, str )
        =>{
			
			 val rt=typecheck(rec)
			 rt match{ case RecordType(t)=>{ t.map{case(x,y)=> if(str.equals(x))
																return NamedType(y)
																		
														}
											}
						case _ => throw new Error("Incorrect attribute "+str) 					
			 }
			 
			expandType(rt)//else throw new Error("Incorrect attribute "+str) 
			  
			}	
		
      case _ => throw new Error("Wrong lvalue: "+e)
    } )

	
	
  /** typecheck the body of a function */
  
  def typecheck ( e: Stmt ) {
    trace(e,
          e match {
      case AssignSt(d,s)
        => if (!typeEquivalence(typecheck(d),typecheck(s)))
               throw new Error("Incompatible types in assignment: "+e)

      /* PUT YOUR CODE HERE */	
	  
	  
	  case CallSt( n, arg)						//TypeCheck number and type of parameters 
		=>{ st.lookup(n) match{
			case Some(ProcDec(otp,params,_,_,_))	
			=>{			
				if(params.length != arg.length )
				throw new Error("Number of parameter doesn't match")
				else (arg.map(typecheck(_)).zip(params)).map({
					case (atp:Type,(_,ptp:Type))
					=>if(!typeEquivalence(atp,ptp))
						throw new Error("The type of call argument ("+atp+") does not match the type of the formal parameter: "+ptp)
				})	
				
			
			}
			case _ => throw new Error("Undefined function: "+n) 
		
			
			}
		}
		
	case ReadSt ( args )			//The l-values in Read() must have type integer or real
		=>{
		
			args.map(typecheck(_)).map{case(x)=> if(typeEquivalence(intType,x)){}
													
												else if(typeEquivalence(floatType,x)){}
														
												else throw new Error("Incorrect Read variable : "+x)	
										}	
		}	
		
		case WriteSt( arg )			//must be simple integers, reals, booleans, or string literals		
		=>{
		
			arg.map(typecheck(_)).map{case(x)=> if(typeEquivalence(intType,x)){}
													
												else if(typeEquivalence(floatType,x)){}
														
												else if(typeEquivalence(stringType,x)){}
													
												else if(typeEquivalence(boolType,x)){}
													
												else throw new Error("Incorrect Write Expression : "+x)	

									}
			
		}  
	  
		case  IfSt ( c, thst, est )    //c must evaluate to a boolean,
		=>{
			val cexp=typecheck(c)
				if(!typeEquivalence(cexp,boolType))
					throw new Error("Incorrect Expression in for if : "+c)
			val t=typecheck(thst)
			val t2=typecheck(est)
		}
		
		case  WhileSt ( cond, body ) 
		=>{
			val wexp=typecheck(cond)
				if(!typeEquivalence(wexp,boolType))
					throw new Error("Incorrect Expression in for While : "+cond)
			typecheck(body)
			
		}
		
		case  LoopSt ( body) 
		=>{
			val l=typecheck(body)
		
		} 
		
		case  ForSt( v, iexp, step, incexp, bdy )
		=>{	
				st.begin_scope()	
				
					st.insert(v,VarDec(intType,0,0))
			
			
					val inexp=typecheck(iexp)
					val stp=typecheck(step)
					val increxp=typecheck(incexp)	
					typecheck(bdy)
			
				if(typeEquivalence(intType,inexp)){}
					
				else throw new Error("Incorrect Expression in for : "+inexp)	
				if(typeEquivalence(intType,stp)){}
					
				else throw new Error("Incorrect Expression in for : "+stp)	
				if(typeEquivalence(intType,increxp)){}
					
				else throw new Error("Incorrect Expression in for : "+increxp)		
			
			st.end_scope()
		}
		
		case  ReturnValueSt ( valexp ) 
		=>{
			val vexp=typecheck(valexp)
			if(typeEquivalence(expected_returned_type,vexp)){}
				
			else throw new Error("Incorrect return type for this procedure : "+vexp)			
		}
		
		
		case  ExitSt () 
		=>{
		}
		
		
		
		case  ReturnSt () 
		=>{
			
			
		}

		
		case  SeqSt ( stmts ) 
		=>{
			stmts.map(typecheck(_))
			
		}
		
	  
	
      case _ => throw new Error("Wrong statement: "+e)
    } )
  }

  
  
  
  /** typecheck the body of a function */
  
  def typecheck ( e: Body, returned: Type ) {
    trace(e,
          e match {
      case Body(ds,s) => {
        ds.foreach(typecheck(_))
        expected_returned_type = returned
        s.foreach(typecheck(_))
      }
    } )
  }

  
  
  
  /** typecheck a declaration block */
  
  def typecheck ( e: Declaration ) {
    trace(e,
          e match {
      case TypeDecls(tds)
        => for ( TypeDecl(n,t) <- tds )
              st.insert(n,TypeDec(t))
      case VarDecls(vds)
        => for ( VarDecl(vs,t,u) <- vds; v <- vs )
                if (t == "NoType")
                  st.insert(v,VarDec(typecheck(u),0,0))
                else if (!typeEquivalence(typecheck(u),NamedType(t)))
                       throw new Error("Incompatible types in variable declaration: "+e)
                else st.insert(v,VarDec(NamedType(t),0,0))
      case ProcDecls(pds) => {
        for ( ProcDecl(f,ot,ps,b) <- pds )
            st.insert(f,ProcDec(NamedType(ot),
                                ps.flatMap({
                                    case (vs,t) => vs.map(_ -> NamedType(t))
                                }),"",0,0))
        for ( ProcDecl(f,ot,ps,b) <- pds ) {
          st.begin_scope()
          for ( (vs,t) <- ps; v <- vs )
              st.insert(v,VarDec(NamedType(t),0,0))
          typecheck(b,NamedType(ot))
          st.end_scope()
        }
      }
    } )
  }

  
  
	/** typecheck the main program */
  
  def typecheck ( e: ProcDecl ) {
    try {
      e match {
        case ProcDecl(f,ot,ps,b) => {
            st.begin_scope()
            typecheck(b,NamedType(ot))
            st.end_scope()
        }
      }
    } catch {
        case e: Error => println("*** Type checking error: " + e)
        sys.exit(-1)
    }
  }
}
