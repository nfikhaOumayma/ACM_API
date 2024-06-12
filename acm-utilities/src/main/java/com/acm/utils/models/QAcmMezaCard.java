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
 * QAcmMezaCard is a Querydsl query type for AcmMezaCard.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmMezaCard extends EntityPathBase<AcmMezaCard> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 395775011L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmMezaCard. */
	public static final QAcmMezaCard acmMezaCard = new QAcmMezaCard("acmMezaCard");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The account. */
	public final StringPath account = createString("account");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The activity date. */
	public final DateTimePath<java.util.Date> activityDate =
			createDateTime("activityDate", java.util.Date.class);

	/** The branch ID. */
	public final NumberPath<Long> branchID = createNumber("branchID", Long.class);

	/** The branch name. */
	public final StringPath branchName = createString("branchName");

	/** The card number. */
	public final StringPath cardNumber = createString("cardNumber");

	/** The card type. */
	public final StringPath cardType = createString("cardType");

	/** The customer. */
	public final QCustomer customer;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The embossed name. */
	public final StringPath embossedName = createString("embossedName");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The expirty date. */
	public final DateTimePath<java.util.Date> expirtyDate =
			createDateTime("expirtyDate", java.util.Date.class);

	/** The id meza card. */
	public final NumberPath<Long> idMezaCard = createNumber("idMezaCard", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The merchant ID. */
	public final NumberPath<java.math.BigDecimal> merchantID =
			createNumber("merchantID", java.math.BigDecimal.class);

	/** The status. */
	public final StringPath status = createString("status");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm meza card.
	 *
	 * @param variable the variable
	 */
	public QAcmMezaCard(String variable) {

		this(AcmMezaCard.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm meza card.
	 *
	 * @param path the path
	 */
	public QAcmMezaCard(Path<? extends AcmMezaCard> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm meza card.
	 *
	 * @param metadata the metadata
	 */
	public QAcmMezaCard(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm meza card.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmMezaCard(PathMetadata metadata, PathInits inits) {

		this(AcmMezaCard.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm meza card.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmMezaCard(Class<? extends AcmMezaCard> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.customer =
				inits.isInitialized("customer") ? new QCustomer(forProperty("customer")) : null;
	}

}
