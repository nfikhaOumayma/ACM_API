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
 * QSettingDocumentType is a Querydsl query type for SettingDocumentType.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingDocumentType extends EntityPathBase<SettingDocumentType> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1801225106L;

	/** The Constant settingDocumentType. */
	public static final QSettingDocumentType settingDocumentType =
			new QSettingDocumentType("settingDocumentType");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm documents. */
	public final SetPath<AcmDocuments, QAcmDocuments> acmDocuments =
			this.<AcmDocuments, QAcmDocuments>createSet("acmDocuments", AcmDocuments.class,
					QAcmDocuments.class, PathInits.DIRECT2);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The categorie. */
	public final NumberPath<Integer> categorie = createNumber("categorie", Integer.class);

	/** The code. */
	public final StringPath code = createString("code");

	/** The date debut. */
	public final DateTimePath<java.util.Date> dateDebut =
			createDateTime("dateDebut", java.util.Date.class);

	/** The date fin. */
	public final DateTimePath<java.util.Date> dateFin =
			createDateTime("dateFin", java.util.Date.class);

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

	/** The setting document products. */
	public final SetPath<SettingDocumentProduct, QSettingDocumentProduct> settingDocumentProducts =
			this.<SettingDocumentProduct, QSettingDocumentProduct>createSet(
					"settingDocumentProducts", SettingDocumentProduct.class,
					QSettingDocumentProduct.class, PathInits.DIRECT2);

	/** The uniqueness. */
	public final BooleanPath uniqueness = createBoolean("uniqueness");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting document type.
	 *
	 * @param variable the variable
	 */
	public QSettingDocumentType(String variable) {

		super(SettingDocumentType.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting document type.
	 *
	 * @param path the path
	 */
	public QSettingDocumentType(Path<? extends SettingDocumentType> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting document type.
	 *
	 * @param metadata the metadata
	 */
	public QSettingDocumentType(PathMetadata metadata) {

		super(SettingDocumentType.class, metadata);
	}

}
