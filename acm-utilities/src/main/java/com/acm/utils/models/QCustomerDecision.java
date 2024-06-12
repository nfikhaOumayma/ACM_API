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
 * QCustomerDecision is a Querydsl query type for CustomerDecision.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCustomerDecision extends EntityPathBase<CustomerDecision> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -779937935L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant customerDecision. */
	public static final QCustomerDecision customerDecision =
			new QCustomerDecision("customerDecision");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The amount. */
	public final NumberPath<java.math.BigDecimal> amount =
			createNumber("amount", java.math.BigDecimal.class);

	/** The comments. */
	public final StringPath comments = createString("comments");

	/** The contact date. */
	public final DateTimePath<java.util.Date> contactDate =
			createDateTime("contactDate", java.util.Date.class);

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

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan. */
	public final QLoan loan;

	/** The status id. */
	public final NumberPath<Integer> statusId = createNumber("statusId", Integer.class);

	/** The status libelle. */
	public final StringPath statusLibelle = createString("statusLibelle");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q customer decision.
	 *
	 * @param variable the variable
	 */
	public QCustomerDecision(String variable) {

		this(CustomerDecision.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q customer decision.
	 *
	 * @param path the path
	 */
	public QCustomerDecision(Path<? extends CustomerDecision> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q customer decision.
	 *
	 * @param metadata the metadata
	 */
	public QCustomerDecision(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q customer decision.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QCustomerDecision(PathMetadata metadata, PathInits inits) {

		this(CustomerDecision.class, metadata, inits);
	}

	/**
	 * Instantiates a new q customer decision.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QCustomerDecision(Class<? extends CustomerDecision> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}
