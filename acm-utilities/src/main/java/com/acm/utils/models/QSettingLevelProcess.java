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
 * QSettingLevelProcess is a Querydsl query type for SettingLevelProcess.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingLevelProcess extends EntityPathBase<SettingLevelProcess> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 641567684L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant settingLevelProcess. */
	public static final QSettingLevelProcess settingLevelProcess =
			new QSettingLevelProcess("settingLevelProcess");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The amount. */
	public final NumberPath<java.math.BigDecimal> amount =
			createNumber("amount", java.math.BigDecimal.class);

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

	/** The id product. */
	public final NumberPath<Long> idProduct = createNumber("idProduct", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The setting level. */
	public final QSettingLevel settingLevel;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting level process.
	 *
	 * @param variable the variable
	 */
	public QSettingLevelProcess(String variable) {

		this(SettingLevelProcess.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q setting level process.
	 *
	 * @param path the path
	 */
	public QSettingLevelProcess(Path<? extends SettingLevelProcess> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q setting level process.
	 *
	 * @param metadata the metadata
	 */
	public QSettingLevelProcess(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q setting level process.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QSettingLevelProcess(PathMetadata metadata, PathInits inits) {

		this(SettingLevelProcess.class, metadata, inits);
	}

	/**
	 * Instantiates a new q setting level process.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QSettingLevelProcess(Class<? extends SettingLevelProcess> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.settingLevel =
				inits.isInitialized("settingLevel") ? new QSettingLevel(forProperty("settingLevel"))
						: null;
	}

}
