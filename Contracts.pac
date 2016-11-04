| package |
package := Package name: 'Kontrakty'.
package paxVersion: 1;
	basicComment: 'ContractBuilder przechowuje kontrakty w mapie postaci "klasa -> kontrakt". Kontrakt dla danej klasy przechowywany w budowniczym zawiera tylko informacje o jego zmianach wzgledem kontraktu z nadklasy.

contractFor generuje pelen, niezalezny kontrakt dla klasy przekazanego obiektu. Tworzy on na poczatku pusty kontrakt, ktory jest pozniej rekurencyjnie modyfikowany w trakcie przechodzenia przez kolejne nadklasy (z gory na dol, konczymy na klasie, dla ktorej generujemy kontrakt). W przypadku jakichkolwiek bledow w strukturze kontraktu (np. podklasa chce usunac warunek, ktorego nie ma w nadklasie) zostanie rzucony odpowiedni wyjatek.

Autor: Tomasz Kotarski'.


package classNames
	add: #ClassContract;
	add: #ContractBuilder;
	add: #ContractViolation;
	add: #FullClassContract;
	add: #FullMethodContract;
	add: #Instrument;
	add: #InvariantViolation;
	add: #MethodConditionViolation;
	add: #MethodContract;
	add: #PostconditionViolation;
	add: #PreconditionViolation;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Smalltalk\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #ClassContract
	instanceVariableNames: 'addedInvs removedInvs methodsMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ContractBuilder
	instanceVariableNames: 'classesMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #FullClassContract
	instanceVariableNames: 'invs methodsMap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #FullMethodContract
	instanceVariableNames: 'precons postcons'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MethodContract
	instanceVariableNames: 'addedPrecons removedPrecons addedPostcons removedPostcons'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #ContractViolation
	instanceVariableNames: 'object condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContractViolation subclass: #InvariantViolation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ContractViolation subclass: #MethodConditionViolation
	instanceVariableNames: 'method'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MethodConditionViolation subclass: #PostconditionViolation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MethodConditionViolation subclass: #PreconditionViolation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ProtoObject subclass: #Instrument
	instanceVariableNames: 'contract object'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ClassContract guid: (GUID fromString: '{D09BFFE1-7819-48EA-A7E2-74D678230BB6}')!
ClassContract comment: ''!
!ClassContract categoriesForClass!Unclassified! !
!ClassContract methodsFor!

addedInvs
	^addedInvs!

addedInvs: i
	addedInvs := i!

addInvariant: i
	(self containsInvariant: i) ifTrue: [Error signal: 'Adding invariant which is already set.'].
	addedInvs add: i!

containsInvariant: i
	^((addedInvs identityIncludes: i) | (removedInvs identityIncludes: i))!

method: methodName
	(methodsMap includesKey: methodName) ifFalse: [methodsMap at: methodName put: (MethodContract new)].
	^methodsMap at: methodName!

methodsMap
	^methodsMap!

methodsMap: m
	methodsMap := m!

removedInvs
	^removedInvs!

removedInvs: i
	removedInvs := i!

removeInvariant: i
	(self containsInvariant: i) ifTrue: [Error signal: 'Removing invariant which is already set.'].
	removedInvs add: i! !
!ClassContract categoriesFor: #addedInvs!public! !
!ClassContract categoriesFor: #addedInvs:!public! !
!ClassContract categoriesFor: #addInvariant:!public! !
!ClassContract categoriesFor: #containsInvariant:!public! !
!ClassContract categoriesFor: #method:!public! !
!ClassContract categoriesFor: #methodsMap!public! !
!ClassContract categoriesFor: #methodsMap:!public! !
!ClassContract categoriesFor: #removedInvs!public! !
!ClassContract categoriesFor: #removedInvs:!public! !
!ClassContract categoriesFor: #removeInvariant:!public! !

!ClassContract class methodsFor!

new
	^super new addedInvs: IdentitySet new; removedInvs: IdentitySet new; methodsMap: Dictionary new! !
!ClassContract class categoriesFor: #new!public! !

ContractBuilder guid: (GUID fromString: '{0E9891BB-A558-492C-8E64-486D5BF9A943}')!
ContractBuilder comment: ''!
!ContractBuilder categoriesForClass!Unclassified! !
!ContractBuilder methodsFor!

class: className
	(classesMap includesKey: className) ifFalse: [classesMap at: className put: (ClassContract new)].
	^classesMap at: className!

classesMap
	^classesMap!

classesMap: m
	classesMap := m!

contractFor: objectName
	^self getFullContract: (objectName class)!

getFullContract: className
	|contract classContract contractMethod|
	(className = nil) ifTrue: [^(FullClassContract new)] ifFalse: [contract := self getFullContract: (className superclass)].	
	(classesMap includesKey: className) ifFalse: [^contract].
	
	classContract := classesMap at: className.
	classContract addedInvs do: [:inv | contract addInvariant: inv].
	classContract removedInvs do: [:inv | contract removeInvariant: inv].
	classContract methodsMap keysAndValuesDo: [:methodName :methodContract |
		contractMethod := contract method: methodName.
		
		methodContract addedPrecons do: [:precon | contractMethod addPrecondition: precon].
		methodContract removedPrecons do: [:precon | contractMethod removePrecondition: precon].

		methodContract addedPostcons do: [:postcon | contractMethod addPostcondition: postcon].
		methodContract removedPostcons do: [:postcon | contractMethod removePostcondition: postcon].
	].

	^contract
! !
!ContractBuilder categoriesFor: #class:!public! !
!ContractBuilder categoriesFor: #classesMap!public! !
!ContractBuilder categoriesFor: #classesMap:!public! !
!ContractBuilder categoriesFor: #contractFor:!public! !
!ContractBuilder categoriesFor: #getFullContract:!public! !

!ContractBuilder class methodsFor!

new
	^super new classesMap: (Dictionary new)! !
!ContractBuilder class categoriesFor: #new!public! !

FullClassContract guid: (GUID fromString: '{D30D9092-6DCD-473E-8A5B-23869DC072BD}')!
FullClassContract comment: ''!
!FullClassContract categoriesForClass!Unclassified! !
!FullClassContract methodsFor!

addInvariant: i
	(invs identityIncludes: i) ifTrue: [Error signal: 'Adding invariant already added.'].
	invs add: i!

containsInvariant: i
	^(invs identityIncludes: i)!

invs
	^invs!

invs: i
	invs := i!

method: methodName
	(methodsMap includesKey: methodName) ifFalse: [methodsMap at: methodName put: (FullMethodContract new)].
	^methodsMap at: methodName!

methodsMap
	^methodsMap!

methodsMap: m
	methodsMap := m!

removeInvariant: i
	(invs identityIncludes: i) ifFalse: [Error signal: 'Removing a non-existing invariant.'].
	invs remove: i! !
!FullClassContract categoriesFor: #addInvariant:!public! !
!FullClassContract categoriesFor: #containsInvariant:!public! !
!FullClassContract categoriesFor: #invs!public! !
!FullClassContract categoriesFor: #invs:!public! !
!FullClassContract categoriesFor: #method:!public! !
!FullClassContract categoriesFor: #methodsMap!public! !
!FullClassContract categoriesFor: #methodsMap:!public! !
!FullClassContract categoriesFor: #removeInvariant:!public! !

!FullClassContract class methodsFor!

new
	^super new invs: IdentitySet new; methodsMap: Dictionary new! !
!FullClassContract class categoriesFor: #new!public! !

FullMethodContract guid: (GUID fromString: '{8497951D-E0F0-465A-9BF7-CAE0FBACE8D2}')!
FullMethodContract comment: ''!
!FullMethodContract categoriesForClass!Unclassified! !
!FullMethodContract methodsFor!

addPostcondition: p
	(postcons identityIncludes: p) ifTrue: [Error signal: 'Adding postcondition already added.'].
	postcons add: p!

addPrecondition: p
	(precons identityIncludes: p) ifTrue: [Error signal: 'Adding precondition already added.'].
	precons add: p!

containsPostcon: p
	^(postcons identityIncludes: p)!

containsPrecon: p
	^(precons identityIncludes: p)!

postcons
	^postcons!

postcons: p
	postcons := p!

precons
	^precons!

precons: p
	precons := p!

removePostcondition: p
	(postcons identityIncludes: p) ifFalse: [Error signal: 'Removing a non-existing postcondition.'].
	postcons remove: p!

removePrecondition: p
	(precons identityIncludes: p) ifFalse: [Error signal: 'Removing a non-existing precondition.'].
	precons remove: p! !
!FullMethodContract categoriesFor: #addPostcondition:!public! !
!FullMethodContract categoriesFor: #addPrecondition:!public! !
!FullMethodContract categoriesFor: #containsPostcon:!public! !
!FullMethodContract categoriesFor: #containsPrecon:!public! !
!FullMethodContract categoriesFor: #postcons!public! !
!FullMethodContract categoriesFor: #postcons:!public! !
!FullMethodContract categoriesFor: #precons!public! !
!FullMethodContract categoriesFor: #precons:!public! !
!FullMethodContract categoriesFor: #removePostcondition:!public! !
!FullMethodContract categoriesFor: #removePrecondition:!public! !

!FullMethodContract class methodsFor!

new
	^super new precons: IdentitySet new; postcons: IdentitySet new! !
!FullMethodContract class categoriesFor: #new!public! !

MethodContract guid: (GUID fromString: '{D390EE8E-5806-4AD7-9B61-3CA11F32CC63}')!
MethodContract comment: ''!
!MethodContract categoriesForClass!Unclassified! !
!MethodContract methodsFor!

addedPostcons
	^addedPostcons!

addedPostcons: p
	addedPostcons := p!

addedPrecons
	^addedPrecons!

addedPrecons: p
	addedPrecons := p!

addPostcondition: c
	(self containsPostcon: c) ifTrue: [Error signal: 'Adding postcondition which is already set.'].
	addedPostcons add: c!

addPrecondition: c
	(self containsPrecon: c) ifTrue: [Error signal: 'Adding precondition which is already set.'].
	addedPrecons add: c!

containsPostcon: c
	^((addedPostcons identityIncludes: c) | (removedPostcons identityIncludes: c))!

containsPrecon: c
	^((addedPrecons identityIncludes: c) | (removedPrecons identityIncludes: c))!

removedPostcons
	^removedPostcons!

removedPostcons: p
	removedPostcons := p!

removedPrecons
	^removedPrecons!

removedPrecons: p
	removedPrecons := p!

removePostcondition: c
	(self containsPostcon: c) ifTrue: [Error signal: 'Removing postcondition which is already set.'].
	removedPostcons add: c!

removePrecondition: c
	(self containsPrecon: c) ifTrue: [Error signal: 'Removing precondition which is already set.'].
	removedPrecons add: c! !
!MethodContract categoriesFor: #addedPostcons!public! !
!MethodContract categoriesFor: #addedPostcons:!public! !
!MethodContract categoriesFor: #addedPrecons!public! !
!MethodContract categoriesFor: #addedPrecons:!public! !
!MethodContract categoriesFor: #addPostcondition:!public! !
!MethodContract categoriesFor: #addPrecondition:!public! !
!MethodContract categoriesFor: #containsPostcon:!public! !
!MethodContract categoriesFor: #containsPrecon:!public! !
!MethodContract categoriesFor: #removedPostcons!public! !
!MethodContract categoriesFor: #removedPostcons:!public! !
!MethodContract categoriesFor: #removedPrecons!public! !
!MethodContract categoriesFor: #removedPrecons:!public! !
!MethodContract categoriesFor: #removePostcondition:!public! !
!MethodContract categoriesFor: #removePrecondition:!public! !

!MethodContract class methodsFor!

new
	^super new addedPrecons: IdentitySet new; addedPostcons: IdentitySet new; removedPrecons: IdentitySet new; removedPostcons: IdentitySet new! !
!MethodContract class categoriesFor: #new!public! !

ContractViolation guid: (GUID fromString: '{8D0CD139-39B7-4677-B47B-99B8F6CC2DAE}')!
ContractViolation comment: ''!
!ContractViolation categoriesForClass!Unclassified! !
!ContractViolation methodsFor!

condition
	^condition!

condition: c
	condition := c!

isResumable
	^true!

object
	^object!

object: obj
	object := obj! !
!ContractViolation categoriesFor: #condition!public! !
!ContractViolation categoriesFor: #condition:!public! !
!ContractViolation categoriesFor: #isResumable!public! !
!ContractViolation categoriesFor: #object!public! !
!ContractViolation categoriesFor: #object:!public! !

InvariantViolation guid: (GUID fromString: '{487D76B8-DBF1-4545-A605-E19DFE8AF331}')!
InvariantViolation comment: ''!
!InvariantViolation categoriesForClass!Unclassified! !
MethodConditionViolation guid: (GUID fromString: '{9EF6E39E-9F8C-4B3D-BB05-50784E0107B7}')!
MethodConditionViolation comment: ''!
!MethodConditionViolation categoriesForClass!Unclassified! !
!MethodConditionViolation methodsFor!

method
	^method!

method: m
	method := m! !
!MethodConditionViolation categoriesFor: #method!public! !
!MethodConditionViolation categoriesFor: #method:!public! !

PostconditionViolation guid: (GUID fromString: '{F32ED023-4389-40CD-95B2-569C41D10F54}')!
PostconditionViolation comment: ''!
!PostconditionViolation categoriesForClass!Unclassified! !
PreconditionViolation guid: (GUID fromString: '{0C64A53D-EF6A-4695-B3B2-73C9D4887308}')!
PreconditionViolation comment: ''!
!PreconditionViolation categoriesForClass!Unclassified! !
Instrument guid: (GUID fromString: '{B87B9284-FF43-4A03-AF7F-BDEA5BA4A57C}')!
Instrument comment: ''!
!Instrument categoriesForClass!Unclassified! !
!Instrument methodsFor!

checkInvariants
	|exception|
	contract invs do: [:inv |
		(inv value: object) ifFalse: [
			exception := InvariantViolation new.
			exception object: object; condition: inv.
			exception signal: 'Invariant violation'
		].
	].!

contract
	^contract!

contract: c
	contract := c!

doesNotUnderstand: msg
	|exception methodResult|
	self checkInvariants.
	(contract method: (msg selector)) precons do: [:precon |
		(precon valueWithArguments: ((Array with: object), msg arguments)) ifFalse: [
			exception := PreconditionViolation new.
			exception object: object; condition: precon; method: (msg selector).
			exception signal: 'Precondition violation'
		].
	].
	
	methodResult := msg forwardTo: object.

	self checkInvariants.
	(contract method: (msg selector)) postcons do: [:postcon |
		(postcon valueWithArguments: ((Array with: object), msg arguments, (Array with: methodResult))) ifFalse: [
			exception := PostconditionViolation new.
			exception object: object; condition: postcon; method: (msg selector).
			exception signal: 'Postcondition violation'
		].
	].
	
	^methodResult!

object
	^object!

object: obj
	object := obj! !
!Instrument categoriesFor: #checkInvariants!public! !
!Instrument categoriesFor: #contract!public! !
!Instrument categoriesFor: #contract:!public! !
!Instrument categoriesFor: #doesNotUnderstand:!public! !
!Instrument categoriesFor: #object!public! !
!Instrument categoriesFor: #object:!public! !

!Instrument class methodsFor!

contract: c on: obj
	^super new contract: c; object: obj! !
!Instrument class categoriesFor: #contract:on:!public! !

"Binary Globals"!

