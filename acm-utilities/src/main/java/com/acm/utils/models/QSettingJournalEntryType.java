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
 * QSettingJournalEntry is a Querydsl query type for SettingJournalEntry.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingJournalEntryType extends EntityPathBase<SettingJournalEntryType> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1048706828L;

	/** The Constant settingJournalEntry. */
	public static final QSettingJournalEntryType settingJournalEntry =
			new QSettingJournalEntryType("settingJournalEntry");

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

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting journal entry type.
	 *
	 * @param variable the variable
	 */
	public QSettingJournalEntryType(String variable) {

		super(SettingJournalEntryType.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting journal entry type.
	 *
	 * @param path the path
	 */
	public QSettingJournalEntryType(Path<? extends SettingJournalEntryType> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting journal entry type.
	 *
	 * @param metadata the metadata
	 */
	public QSettingJournalEntryType(PathMetadata metadata) {

		super(SettingJournalEntryType.class, metadata);
	}

}
