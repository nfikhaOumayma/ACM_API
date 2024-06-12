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
 * QIncentiveOperation is a Querydsl query type for IncentiveOperation.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveOperation extends EntityPathBase<IncentiveOperation> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -851314669L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant incentiveOperation. */
	public static final QIncentiveOperation incentiveOperation =
			new QIncentiveOperation("incentiveOperation");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

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

	/** The role. */
	public final StringPath role = createString("role");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q incentive operation.
	 *
	 * @param variable the variable
	 */
	public QIncentiveOperation(String variable) {

		this(IncentiveOperation.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q incentive operation.
	 *
	 * @param path the path
	 */
	public QIncentiveOperation(Path<? extends IncentiveOperation> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q incentive operation.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveOperation(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q incentive operation.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveOperation(PathMetadata metadata, PathInits inits) {

		this(IncentiveOperation.class, metadata, inits);
	}

	/**
	 * Instantiates a new q incentive operation.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveOperation(Class<? extends IncentiveOperation> type, PathMetadata metadata,
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
