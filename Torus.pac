| package |
package := Package name: 'TomaszKotarski'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #AssWalk;
	add: #BlockWalk;
	add: #ConcatWalk;
	add: #DirWalk;
	add: #OnePointWalk;
	add: #Torus;
	add: #Walk;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Dokumenty\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Torus
	instanceVariableNames: 'values shape point'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Collection subclass: #Walk
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Walk subclass: #AssWalk
	instanceVariableNames: 'subWalk steps direction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Walk subclass: #BlockWalk
	instanceVariableNames: 'torus block'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Walk subclass: #ConcatWalk
	instanceVariableNames: 'subWalk1 subWalk2'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Walk subclass: #DirWalk
	instanceVariableNames: 'subWalk direction'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Walk subclass: #OnePointWalk
	instanceVariableNames: 'torus'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Torus guid: (GUID fromString: '{3C5D115D-2E86-4491-A1B7-06BD36D6CB00}')!
Torus comment: ''!
!Torus categoriesForClass!Kernel-Objects! !
!Torus methodsFor!

- direction
	^self + direction negated!

% pair
	| walk |
	walk := AssWalk basicNew.
	walk
		subWalk: self createWalk
		steps: pair key
		direction: pair value.
	^walk!

& block
	| walk |
	walk := BlockWalk basicNew.
	walk torus: self block: block.
	^walk!

@ vector
	| newPoint |
	newPoint := point copy.
	1 to: vector size do: [:i | newPoint at: i put: ((point at: i) + (vector at: i)) \\ (shape at: i)].
	^self getNewPoint: newPoint!

| direction
	| walk |
	walk := DirWalk basicNew.
	walk subWalk: self createWalk direction: direction.
	^walk!

+ direction
	| newPoint newVal dir |
	direction < 0 ifTrue: [dir := direction negated] ifFalse: [dir := direction].
	direction = 0 ifTrue: [^self].
	newPoint := point copy.
	direction > 0 ifTrue: [newVal := ((point at: dir) + 1) \\ (shape at: dir)].
	direction < 0 ifTrue: [newVal := ((point at: dir) - 1) \\ (shape at: dir)].
	newPoint at: dir put: newVal.
	^self getNewPoint: newPoint!

createWalk
	| walk |
	walk := OnePointWalk basicNew.
	walk torus: self.
	^walk!

getNewPoint: array
	| torus |
	torus := Torus new.
	torus setValues: values.
	torus setShape: shape.
	torus setPoint: array.
	^torus!

getPoint
	^point!

getShape
	^shape!

getValues
	^values!

init: collection
	| colSize |
	colSize := collection size.
	values := Dictionary new.
	shape := Array new: colSize.
	point := Array new: colSize.
	1 to: colSize
		do: 
			[:i |
			shape at: i put: (collection at: i).
			point at: i put: 0]!

printOn: stream
	self value printOn: stream!

setPoint: array
	point := array!

setShape: array
	shape := array!

setValues: dict
	values := dict!

value
	(values includesKey: point) ifTrue: [^values at: point] ifFalse: [^nil]!

value: val
	values at: point put: val! !
!Torus categoriesFor: #-!public! !
!Torus categoriesFor: #%!public! !
!Torus categoriesFor: #&!public! !
!Torus categoriesFor: #@!public! !
!Torus categoriesFor: #|!public! !
!Torus categoriesFor: #+!public! !
!Torus categoriesFor: #createWalk!private! !
!Torus categoriesFor: #getNewPoint:!private! !
!Torus categoriesFor: #getPoint!public! !
!Torus categoriesFor: #getShape!public! !
!Torus categoriesFor: #getValues!public! !
!Torus categoriesFor: #init:!public! !
!Torus categoriesFor: #printOn:!public! !
!Torus categoriesFor: #setPoint:!public! !
!Torus categoriesFor: #setShape:!public! !
!Torus categoriesFor: #setValues:!public! !
!Torus categoriesFor: #value!public! !
!Torus categoriesFor: #value:!public! !

!Torus class methodsFor!

shape: collection
	| torus |
	torus := Torus new.
	torus init: collection.
	^torus! !
!Torus class categoriesFor: #shape:!public! !

Walk guid: (GUID fromString: '{44F08946-8245-495B-BB11-5E561AAF9D1E}')!
Walk comment: ''!
!Walk categoriesForClass!Collections-Abstract! !
!Walk methodsFor!

% x
	| walk |
	walk := AssWalk basicNew.
	walk
		subWalk: self
		steps: x key
		direction: x value.
	^walk!

, t
	| walk |
	walk := ConcatWalk basicNew.
	walk subWalk1: self subWalk2: t.
	^walk!

| x
	| walk |
	walk := DirWalk basicNew.
	walk subWalk: self direction: x.
	^walk!

add: a
	^self shouldNotImplement!

first: anInteger
	| answer i |
	answer := OrderedCollection new.
	anInteger > 0 ifFalse: [^answer].
	i := anInteger.
	self do: 
			[:each |
			answer add: each.
			i := i - 1.
			i = 0 ifTrue: [^answer]].
	^answer!

remove: a ifAbsent: b
	^self shouldNotImplement!

species
	^OrderedCollection! !
!Walk categoriesFor: #%!public! !
!Walk categoriesFor: #,!public! !
!Walk categoriesFor: #|!public! !
!Walk categoriesFor: #add:!public! !
!Walk categoriesFor: #first:!public! !
!Walk categoriesFor: #remove:ifAbsent:!public! !
!Walk categoriesFor: #species!public! !

!Walk class methodsFor!

new
	^self shouldNotImplement!

new: a
	^self shouldNotImplement! !
!Walk class categoriesFor: #new!public! !
!Walk class categoriesFor: #new:!public! !

AssWalk guid: (GUID fromString: '{9DD48A36-5D26-4CC1-9C3E-E48B571BE0F8}')!
AssWalk comment: ''!
!AssWalk categoriesForClass!Collections-Abstract! !
!AssWalk methodsFor!

do: block
	| torus stepsLeft |
	subWalk do: 
			[:p |
			torus := p.
			stepsLeft := steps.
			[stepsLeft = 0] whileFalse: 
					[block value: torus.
					torus := torus + direction.
					stepsLeft := stepsLeft - 1]]!

subWalk: walk steps: s direction: dir
	subWalk := walk.
	steps := s.
	direction := dir! !
!AssWalk categoriesFor: #do:!public! !
!AssWalk categoriesFor: #subWalk:steps:direction:!public! !

BlockWalk guid: (GUID fromString: '{0100D05F-0DB9-488C-A879-89AF8843DF1A}')!
BlockWalk comment: ''!
!BlockWalk categoriesForClass!Collections-Abstract! !
!BlockWalk methodsFor!

do: instr
	| w |
	instr value: torus.
	w := block value: torus.
	w = nil ifFalse: [w do: instr]!

torus: t block: b
	torus := t.
	block := b! !
!BlockWalk categoriesFor: #do:!public! !
!BlockWalk categoriesFor: #torus:block:!public! !

ConcatWalk guid: (GUID fromString: '{49DBBBE4-4E05-4474-AC81-C64FB603E9F3}')!
ConcatWalk comment: ''!
!ConcatWalk categoriesForClass!Collections-Abstract! !
!ConcatWalk methodsFor!

do: block
	subWalk1 do: block.
	subWalk2 do: block!

subWalk1: walk1 subWalk2: walk2
	subWalk1 := walk1.
	subWalk2 := walk2! !
!ConcatWalk categoriesFor: #do:!public! !
!ConcatWalk categoriesFor: #subWalk1:subWalk2:!public! !

DirWalk guid: (GUID fromString: '{2EA7C809-F404-4AC6-B8EF-443567ED537B}')!
DirWalk comment: ''!
!DirWalk categoriesForClass!Collections-Abstract! !
!DirWalk methodsFor!

do: block
	| torus |
	subWalk do: 
			[:p |
			torus := p.
			block value: torus.
			torus := torus + direction.
			[torus getPoint = p getPoint] whileFalse: 
					[block value: torus.
					torus := torus + direction]]!

subWalk: walk direction: dir
	subWalk := walk.
	direction := dir! !
!DirWalk categoriesFor: #do:!public! !
!DirWalk categoriesFor: #subWalk:direction:!public! !

OnePointWalk guid: (GUID fromString: '{1ACCF727-33B5-4D55-B25A-FAD4E402FB8A}')!
OnePointWalk comment: ''!
!OnePointWalk categoriesForClass!Collections-Abstract! !
!OnePointWalk methodsFor!

do: block
	block value: torus!

torus: t
	torus := t! !
!OnePointWalk categoriesFor: #do:!public! !
!OnePointWalk categoriesFor: #torus:!public! !

"Binary Globals"!

