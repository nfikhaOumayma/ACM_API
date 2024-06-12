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
 * QIncentiveSettingRun is a Querydsl query type for IncentiveSettingRun.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveSettingRun extends EntityPathBase<IncentiveSettingRun> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -629078801L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant incentiveSettingRun. */
	public static final QIncentiveSettingRun incentiveSettingRun =
			new QIncentiveSettingRun("incentiveSettingRun");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The appaly branch prod level. */
	public final BooleanPath appalyBranchProdLevel = createBoolean("appalyBranchProdLevel");

	/** The applay discount rule. */
	public final BooleanPath applayDiscountRule = createBoolean("applayDiscountRule");

	/** The code. */
	public final StringPath code = createString("code");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

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

	/** The role. */
	public final StringPath role = createString("role");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q incentive setting run.
	 *
	 * @param variable the variable
	 */
	public QIncentiveSettingRun(String variable) {

		this(IncentiveSettingRun.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q incentive setting run.
	 *
	 * @param path the path
	 */
	public QIncentiveSettingRun(Path<? extends IncentiveSettingRun> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q incentive setting run.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveSettingRun(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q incentive setting run.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveSettingRun(PathMetadata metadata, PathInits inits) {

		this(IncentiveSettingRun.class, metadata, inits);
	}

	/**
	 * Instantiates a new q incentive setting run.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveSettingRun(Class<? extends IncentiveSettingRun> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.frequency = inits.isInitialized("frequency")
				? new QIncentiveSettingConstant(forProperty("frequency"))
				: null;
	}

}
