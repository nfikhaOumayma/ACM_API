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
import com.querydsl.core.types.dsl.StringPath;

/**
 * QSettingRequiredStep is a Querydsl query type for SettingRequiredStep.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingRequiredStep extends EntityPathBase<SettingRequiredStep> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1956746108L;

	/** The Constant settingRequiredStep. */
	public static final QSettingRequiredStep settingRequiredStep =
			new QSettingRequiredStep("settingRequiredStep");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

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

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The mandatory. */
	public final BooleanPath mandatory = createBoolean("mandatory");

	/** The product id. */
	public final NumberPath<Integer> productId = createNumber("productId", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting required step.
	 *
	 * @param variable the variable
	 */
	public QSettingRequiredStep(String variable) {

		super(SettingRequiredStep.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting required step.
	 *
	 * @param path the path
	 */
	public QSettingRequiredStep(Path<? extends SettingRequiredStep> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting required step.
	 *
	 * @param metadata the metadata
	 */
	public QSettingRequiredStep(PathMetadata metadata) {

		super(SettingRequiredStep.class, metadata);
	}

}
