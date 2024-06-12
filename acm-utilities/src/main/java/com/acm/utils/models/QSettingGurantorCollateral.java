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
 * QSettingGurantorCollateral is a Querydsl query type for SettingGurantorCollateral.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingGurantorCollateral extends EntityPathBase<SettingGurantorCollateral> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 652368086L;

	/** The Constant settingGurantorCollateral. */
	public static final QSettingGurantorCollateral settingGurantorCollateral =
			new QSettingGurantorCollateral("settingGurantorCollateral");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

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

	/** The key abacus. */
	public final StringPath keyAbacus = createString("keyAbacus");

	/** The mandatory. */
	public final BooleanPath mandatory = createBoolean("mandatory");

	/** The product id. */
	public final NumberPath<Integer> productId = createNumber("productId", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting gurantor collateral.
	 *
	 * @param variable the variable
	 */
	public QSettingGurantorCollateral(String variable) {

		super(SettingGurantorCollateral.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting gurantor collateral.
	 *
	 * @param path the path
	 */
	public QSettingGurantorCollateral(Path<? extends SettingGurantorCollateral> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting gurantor collateral.
	 *
	 * @param metadata the metadata
	 */
	public QSettingGurantorCollateral(PathMetadata metadata) {

		super(SettingGurantorCollateral.class, metadata);
	}

}
