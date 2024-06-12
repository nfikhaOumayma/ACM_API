/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QAcmCollection is a Querydsl query type for AcmCollection.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmCollection extends EntityPathBase<AcmCollection> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1618440366L;

	/** The Constant acmCollection. */
	public static final QAcmCollection acmCollection = new QAcmCollection("acmCollection");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The account number. */
	public final StringPath accountNumber = createString("accountNumber");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The amount. */
	public final NumberPath<java.math.BigDecimal> amount =
			createNumber("amount", java.math.BigDecimal.class);

	/** The available date. */
	public final DateTimePath<java.util.Date> availableDate =
			createDateTime("availableDate", java.util.Date.class);

	/** The branch description. */
	public final StringPath branchDescription = createString("branchDescription");

	/** The branch id. */
	public final NumberPath<Long> branchId = createNumber("branchId", Long.class);

	/** The collection instances. */
	public final SetPath<CollectionInstance, QCollectionInstance> collectionInstances =
			this.<CollectionInstance, QCollectionInstance>createSet("collectionInstances",
					CollectionInstance.class, QCollectionInstance.class, PathInits.DIRECT2);

	/** The collection type. */
	public final StringPath collectionType = createString("collectionType");

	/** The currency decimal places. */
	public final NumberPath<Integer> currencyDecimalPlaces =
			createNumber("currencyDecimalPlaces", Integer.class);

	/** The currency symbol. */
	public final StringPath currencySymbol = createString("currencySymbol");

	/** The customer id extern. */
	public final NumberPath<Long> customerIdExtern = createNumber("customerIdExtern", Long.class);

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The first unpaid installment. */
	public final DateTimePath<java.util.Date> firstUnpaidInstallment =
			createDateTime("firstUnpaidInstallment", java.util.Date.class);

	/** The group owner. */
	public final StringPath groupOwner = createString("groupOwner");

	/** The group owner name. */
	public final StringPath groupOwnerName = createString("groupOwnerName");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id acm collection step. */
	public final NumberPath<Long> idAcmCollectionStep =
			createNumber("idAcmCollectionStep", Long.class);

	/** The id loan extern. */
	public final NumberPath<Integer> idLoanExtern = createNumber("idLoanExtern", Integer.class);

	/** The id parent collection. */
	public final NumberPath<Long> idParentCollection =
			createNumber("idParentCollection", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The late days. */
	public final NumberPath<Integer> lateDays = createNumber("lateDays", Integer.class);

	/** The loan officer. */
	public final StringPath loanOfficer = createString("loanOfficer");

	/** The number of unpaid installment. */
	public final NumberPath<Integer> numberOfUnpaidInstallment =
			createNumber("numberOfUnpaidInstallment", Integer.class);

	/** The owner. */
	public final StringPath owner = createString("owner");

	/** The owner name. */
	public final StringPath ownerName = createString("ownerName");

	/** The product description. */
	public final StringPath productDescription = createString("productDescription");

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The status. */
	public final NumberPath<Integer> status = createNumber("status", Integer.class);

	/** The statut libelle. */
	public final StringPath statutLibelle = createString("statutLibelle");

	/** The statut libelle done. */
	public final StringPath statutLibelleDone = createString("statutLibelleDone");

	/** The statut workflow. */
	public final StringPath statutWorkflow = createString("statutWorkflow");

	/** The type customer. */
	public final StringPath typeCustomer = createString("typeCustomer");

	/** The unpaid amount. */
	public final NumberPath<java.math.BigDecimal> unpaidAmount =
			createNumber("unpaidAmount", java.math.BigDecimal.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm collection.
	 *
	 * @param variable the variable
	 */
	public QAcmCollection(String variable) {

		super(AcmCollection.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm collection.
	 *
	 * @param path the path
	 */
	public QAcmCollection(Path<? extends AcmCollection> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm collection.
	 *
	 * @param metadata the metadata
	 */
	public QAcmCollection(PathMetadata metadata) {

		super(AcmCollection.class, metadata);
	}

}
