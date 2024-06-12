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
 * QIncentiveRepayment is a Querydsl query type for IncentiveRepayment.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveRepayment extends EntityPathBase<IncentiveRepayment> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1033074463L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant incentiveRepayment. */
	public static final QIncentiveRepayment incentiveRepayment =
			new QIncentiveRepayment("incentiveRepayment");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The active customer id. */
	public final NumberPath<Long> activeCustomerId = createNumber("activeCustomerId", Long.class);

	/** The based on. */
	public final QIncentiveSettingConstant basedOn;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The frequency. */
	public final QIncentiveSettingConstant frequency;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The incentive type. */
	public final QIncentiveSettingConstant incentiveType;

	/** The incentive value. */
	public final StringPath incentiveValue = createString("incentiveValue");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The ordre. */
	public final NumberPath<Long> ordre = createNumber("ordre", Long.class);

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The productivity id. */
	public final NumberPath<Long> productivityId = createNumber("productivityId", Long.class);

	/** The risk level id. */
	public final NumberPath<Long> riskLevelId = createNumber("riskLevelId", Long.class);

	/** The role. */
	public final StringPath role = createString("role");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q incentive repayment.
	 *
	 * @param variable the variable
	 */
	public QIncentiveRepayment(String variable) {

		this(IncentiveRepayment.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q incentive repayment.
	 *
	 * @param path the path
	 */
	public QIncentiveRepayment(Path<? extends IncentiveRepayment> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q incentive repayment.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveRepayment(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q incentive repayment.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveRepayment(PathMetadata metadata, PathInits inits) {

		this(IncentiveRepayment.class, metadata, inits);
	}

	/**
	 * Instantiates a new q incentive repayment.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveRepayment(Class<? extends IncentiveRepayment> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.basedOn = inits.isInitialized("basedOn")
				? new QIncentiveSettingConstant(forProperty("basedOn"))
				: null;
		this.frequency = inits.isInitialized("frequency")
				? new QIncentiveSettingConstant(forProperty("frequency"))
				: null;
		this.incentiveType = inits.isInitialized("incentiveType")
				? new QIncentiveSettingConstant(forProperty("incentiveType"))
				: null;
	}

}
