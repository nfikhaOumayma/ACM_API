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
 * QIncentiveLegal is a Querydsl query type for IncentiveLegal.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveLegal extends EntityPathBase<IncentiveLegal> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 339524453L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant incentiveLegal. */
	public static final QIncentiveLegal incentiveLegal = new QIncentiveLegal("incentiveLegal");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

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

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The ordre. */
	public final NumberPath<Long> ordre = createNumber("ordre", Long.class);

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q incentive legal.
	 *
	 * @param variable the variable
	 */
	public QIncentiveLegal(String variable) {

		this(IncentiveLegal.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q incentive legal.
	 *
	 * @param path the path
	 */
	public QIncentiveLegal(Path<? extends IncentiveLegal> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q incentive legal.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveLegal(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q incentive legal.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveLegal(PathMetadata metadata, PathInits inits) {

		this(IncentiveLegal.class, metadata, inits);
	}

	/**
	 * Instantiates a new q incentive legal.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveLegal(Class<? extends IncentiveLegal> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.frequency = inits.isInitialized("frequency")
				? new QIncentiveSettingConstant(forProperty("frequency"))
				: null;
	}

}
