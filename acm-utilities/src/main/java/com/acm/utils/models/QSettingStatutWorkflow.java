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
 * QSettingStatutWorkflow is a Querydsl query type for SettingStatutWorkflow.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingStatutWorkflow extends EntityPathBase<SettingStatutWorkflow> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1626840075L;

	/** The Constant settingStatutWorkflow. */
	public static final QSettingStatutWorkflow settingStatutWorkflow =
			new QSettingStatutWorkflow("settingStatutWorkflow");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The client. */
	public final StringPath client = createString("client");

	/** The code. */
	public final NumberPath<Integer> code = createNumber("code", Integer.class);

	/** The code statut loan. */
	public final NumberPath<Long> codeStatutLoan = createNumber("codeStatutLoan", Long.class);

	/** The code staut ib. */
	public final NumberPath<Integer> codeStautIb = createNumber("codeStautIb", Integer.class);

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

	/** The ihm root. */
	public final StringPath ihmRoot = createString("ihmRoot");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The order etape process. */
	public final NumberPath<Integer> orderEtapeProcess =
			createNumber("orderEtapeProcess", Integer.class);

	/** The process name. */
	public final StringPath processName = createString("processName");

	/** The show ib. */
	public final BooleanPath showIb = createBoolean("showIb");

	/** The statut loan. */
	public final StringPath statutLoan = createString("statutLoan");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting statut workflow.
	 *
	 * @param variable the variable
	 */
	public QSettingStatutWorkflow(String variable) {

		super(SettingStatutWorkflow.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting statut workflow.
	 *
	 * @param path the path
	 */
	public QSettingStatutWorkflow(Path<? extends SettingStatutWorkflow> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting statut workflow.
	 *
	 * @param metadata the metadata
	 */
	public QSettingStatutWorkflow(PathMetadata metadata) {

		super(SettingStatutWorkflow.class, metadata);
	}

}
