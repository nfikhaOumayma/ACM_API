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
 * QSettingTypeRisque is a Querydsl query type for SettingTypeRisque.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingTypeRisk extends EntityPathBase<SettingTypeRisk> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 249919416L;

	/** The Constant settingTypeRisque. */
	public static final QSettingTypeRisk settingTypeRisque =
			new QSettingTypeRisk("settingTypeRisque");

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

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting type risque.
	 *
	 * @param variable the variable
	 */
	public QSettingTypeRisk(String variable) {

		super(SettingTypeRisk.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting type risque.
	 *
	 * @param path the path
	 */
	public QSettingTypeRisk(Path<? extends SettingTypeRisk> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting type risque.
	 *
	 * @param metadata the metadata
	 */
	public QSettingTypeRisk(PathMetadata metadata) {

		super(SettingTypeRisk.class, metadata);
	}

}
