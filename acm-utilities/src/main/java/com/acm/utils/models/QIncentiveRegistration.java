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
 * QIncentiveRegistration is a Querydsl query type for IncentiveRegistration.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveRegistration extends EntityPathBase<IncentiveRegistration> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -809519475L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant incentiveRegistration. */
	public static final QIncentiveRegistration incentiveRegistration =
			new QIncentiveRegistration("incentiveRegistration");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The based on. */
	public final QIncentiveSettingConstant basedOn;

	/** The customer type. */
	public final QIncentiveSettingConstant customerType;

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
	 * Instantiates a new q incentive registration.
	 *
	 * @param variable the variable
	 */
	public QIncentiveRegistration(String variable) {

		this(IncentiveRegistration.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q incentive registration.
	 *
	 * @param path the path
	 */
	public QIncentiveRegistration(Path<? extends IncentiveRegistration> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q incentive registration.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveRegistration(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q incentive registration.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveRegistration(PathMetadata metadata, PathInits inits) {

		this(IncentiveRegistration.class, metadata, inits);
	}

	/**
	 * Instantiates a new q incentive registration.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIncentiveRegistration(Class<? extends IncentiveRegistration> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.basedOn = inits.isInitialized("basedOn")
				? new QIncentiveSettingConstant(forProperty("basedOn"))
				: null;
		this.customerType = inits.isInitialized("customerType")
				? new QIncentiveSettingConstant(forProperty("customerType"))
				: null;
		this.frequency = inits.isInitialized("frequency")
				? new QIncentiveSettingConstant(forProperty("frequency"))
				: null;
		this.incentiveType = inits.isInitialized("incentiveType")
				? new QIncentiveSettingConstant(forProperty("incentiveType"))
				: null;
	}

}
