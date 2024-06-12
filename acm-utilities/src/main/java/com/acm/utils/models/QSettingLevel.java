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
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QSettingLevel is a Querydsl query type for SettingLevel.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingLevel extends EntityPathBase<SettingLevel> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 58209323L;

	/** The Constant settingLevel. */
	public static final QSettingLevel settingLevel = new QSettingLevel("settingLevel");

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

	/** The level order. */
	public final NumberPath<Integer> levelOrder = createNumber("levelOrder", Integer.class);

	/** The setting level processes. */
	public final SetPath<SettingLevelProcess, QSettingLevelProcess> settingLevelProcesses =
			this.<SettingLevelProcess, QSettingLevelProcess>createSet("settingLevelProcesses",
					SettingLevelProcess.class, QSettingLevelProcess.class, PathInits.DIRECT2);

	/** The title. */
	public final StringPath title = createString("title");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting level.
	 *
	 * @param variable the variable
	 */
	public QSettingLevel(String variable) {

		super(SettingLevel.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting level.
	 *
	 * @param path the path
	 */
	public QSettingLevel(Path<? extends SettingLevel> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting level.
	 *
	 * @param metadata the metadata
	 */
	public QSettingLevel(PathMetadata metadata) {

		super(SettingLevel.class, metadata);
	}

}
