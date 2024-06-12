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
 * QIncentiveSettingBranchProdLevel is a Querydsl query type for IncentiveSettingBranchProdLevel.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveSettingBranchProdLevel
		extends EntityPathBase<IncentiveSettingBranchProdLevel> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -795702449L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant incentiveSettingBranchProdLevel. */
	public static final QIncentiveSettingBranchProdLevel incentiveSettingBranchProdLevel =
			new QIncentiveSettingBranchProdLevel("incentiveSettingBranchProdLevel");

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

	/** The min amount. */
	public final NumberPath<Long> minAmount = createNumber("minAmount", Long.class);

	/** The min number customer. */
	public final NumberPath<Long> minNumberCustomer = createNumber("minNumberCustomer", Long.class);

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
	 * Instantiates a new q incentive setting branch prod level.
	 *
	 * @param variable the variable
	 */
	public QIncentiveSettingBranchProdLevel(String variable) {

		this(IncentiveSettingBranchProdLevel.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q incentive setting branch prod level.
	 *
	 * @param path the path
	 */
	public QIncentiveSettingBranchProdLevel(Path<? extends IncentiveSettingBranchProdLevel> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q incentive setting branch prod level.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveSettingBranchProdLevel(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q incentive setting branch prod level.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveSettingBranchProdLevel(PathMetadata metadata, PathInits inits) {

		this(IncentiveSettingBranchProdLevel.class, metadata, inits);
	}

	/**
	 * Instantiates a new q incentive setting branch prod level.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveSettingBranchProdLevel(Class<? extends IncentiveSettingBranchProdLevel> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.frequency = inits.isInitialized("frequency")
				? new QIncentiveSettingConstant(forProperty("frequency"))
				: null;
	}

}
