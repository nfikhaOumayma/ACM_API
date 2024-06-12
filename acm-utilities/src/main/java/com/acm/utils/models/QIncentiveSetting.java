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
 * QIncentiveSetting is a Querydsl query type for IncentiveSetting.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveSetting extends EntityPathBase<IncentiveSetting> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1795615740L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant incentiveSetting. */
	public static final QIncentiveSetting incentiveSetting =
			new QIncentiveSetting("incentiveSetting");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The category. */
	public final StringPath category = createString("category");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The discount. */
	public final NumberPath<Long> discount = createNumber("discount", Long.class);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The frequency. */
	public final QIncentiveSettingConstant frequency;

	/** The from. */
	public final NumberPath<Long> from = createNumber("from", Long.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The ordre. */
	public final NumberPath<Long> ordre = createNumber("ordre", Long.class);

	/** The product id. */
	public final NumberPath<Long> productId = createNumber("productId", Long.class);

	/** The to. */
	public final NumberPath<Long> to = createNumber("to", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q incentive setting.
	 *
	 * @param variable the variable
	 */
	public QIncentiveSetting(String variable) {

		this(IncentiveSetting.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q incentive setting.
	 *
	 * @param path the path
	 */
	public QIncentiveSetting(Path<? extends IncentiveSetting> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q incentive setting.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveSetting(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q incentive setting.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveSetting(PathMetadata metadata, PathInits inits) {

		this(IncentiveSetting.class, metadata, inits);
	}

	/**
	 * Instantiates a new q incentive setting.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveSetting(Class<? extends IncentiveSetting> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.frequency = inits.isInitialized("frequency")
				? new QIncentiveSettingConstant(forProperty("frequency"))
				: null;
	}

}
