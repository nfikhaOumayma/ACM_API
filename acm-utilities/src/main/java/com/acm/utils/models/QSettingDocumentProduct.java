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
 * QSettingDocumentProduct is a Querydsl query type for SettingDocumentProduct.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingDocumentProduct extends EntityPathBase<SettingDocumentProduct> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1567974203L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant settingDocumentProduct. */
	public static final QSettingDocumentProduct settingDocumentProduct =
			new QSettingDocumentProduct("settingDocumentProduct");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The collections. */
	public final SetPath<CollectionStep, QCollectionStep> collections =
			this.<CollectionStep, QCollectionStep>createSet("collections", CollectionStep.class,
					QCollectionStep.class, PathInits.DIRECT2);

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

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The Loans. */
	public final SetPath<WorkFlowStep, QWorkFlowStep> Loans =
			this.<WorkFlowStep, QWorkFlowStep>createSet("Loans", WorkFlowStep.class,
					QWorkFlowStep.class, PathInits.DIRECT2);

	/** The mandatory. */
	public final BooleanPath mandatory = createBoolean("mandatory");

	/** The product id. */
	public final NumberPath<Integer> productId = createNumber("productId", Integer.class);

	/** The report name. */
	public final StringPath reportName = createString("reportName");

	/** The setting document type. */
	public final QSettingDocumentType settingDocumentType;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting document product.
	 *
	 * @param variable the variable
	 */
	public QSettingDocumentProduct(String variable) {

		this(SettingDocumentProduct.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q setting document product.
	 *
	 * @param path the path
	 */
	public QSettingDocumentProduct(Path<? extends SettingDocumentProduct> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q setting document product.
	 *
	 * @param metadata the metadata
	 */
	public QSettingDocumentProduct(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q setting document product.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QSettingDocumentProduct(PathMetadata metadata, PathInits inits) {

		this(SettingDocumentProduct.class, metadata, inits);
	}

	/**
	 * Instantiates a new q setting document product.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QSettingDocumentProduct(Class<? extends SettingDocumentProduct> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.settingDocumentType = inits.isInitialized("settingDocumentType")
				? new QSettingDocumentType(forProperty("settingDocumentType"))
				: null;
	}

}
