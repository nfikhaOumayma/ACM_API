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
import com.querydsl.core.types.dsl.StringPath;

/**
 * QCustomerLinksRelationship is a Querydsl query type for CustomerLinksRelationship.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCustomerLinksRelationship extends EntityPathBase<CustomerLinksRelationship> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1765008228L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant customerLinksRelationship. */
	public static final QCustomerLinksRelationship customerLinksRelationship =
			new QCustomerLinksRelationship("customerLinksRelationship");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The amount guarantor. */
	public final NumberPath<java.math.BigDecimal> amountGuarantor =
			createNumber("amountGuarantor", java.math.BigDecimal.class);

	/** The category. */
	public final StringPath category = createString("category");

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The date debut. */
	public final DateTimePath<java.util.Date> dateDebut =
			createDateTime("dateDebut", java.util.Date.class);

	/** The date fin. */
	public final DateTimePath<java.util.Date> dateFin =
			createDateTime("dateFin", java.util.Date.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id loan. */
	public final NumberPath<Long> idLoan = createNumber("idLoan", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The link relationship type. */
	public final StringPath linkRelationshipType = createString("linkRelationshipType");

	/** The member. */
	public final QCustomer member;

	/** The percentage owned. */
	public final NumberPath<java.math.BigDecimal> percentageOwned =
			createNumber("percentageOwned", java.math.BigDecimal.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q customer links relationship.
	 *
	 * @param variable the variable
	 */
	public QCustomerLinksRelationship(String variable) {

		this(CustomerLinksRelationship.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q customer links relationship.
	 *
	 * @param path the path
	 */
	public QCustomerLinksRelationship(Path<? extends CustomerLinksRelationship> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q customer links relationship.
	 *
	 * @param metadata the metadata
	 */
	public QCustomerLinksRelationship(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q customer links relationship.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QCustomerLinksRelationship(PathMetadata metadata, PathInits inits) {

		this(CustomerLinksRelationship.class, metadata, inits);
	}

	/**
	 * Instantiates a new q customer links relationship.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QCustomerLinksRelationship(Class<? extends CustomerLinksRelationship> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.member = inits.isInitialized("member") ? new QCustomer(forProperty("member")) : null;
	}

}
