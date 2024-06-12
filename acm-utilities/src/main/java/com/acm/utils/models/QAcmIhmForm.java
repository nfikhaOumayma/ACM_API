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

import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;

/**
 * QAcmIhmForm is a Querydsl query type for AcmIhmForm.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmIhmForm extends EntityPathBase<AcmIhmForm> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1246481630L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmIhmForm. */
	public static final QAcmIhmForm acmIhmForm = new QAcmIhmForm("acmIhmForm");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm ihm fields. */
	public final SetPath<AcmIhmField, QAcmIhmField> acmIhmFields =
			this.<AcmIhmField, QAcmIhmField>createSet("acmIhmFields", AcmIhmField.class,
					QAcmIhmField.class, PathInits.DIRECT2);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The code page. */
	public final StringPath codePage = createString("codePage");

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

	/** The habilitation IHM route. */
	public final QHabilitationIHMRoute habilitationIHMRoute;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm ihm form.
	 *
	 * @param variable the variable
	 */
	public QAcmIhmForm(String variable) {

		this(AcmIhmForm.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm ihm form.
	 *
	 * @param path the path
	 */
	public QAcmIhmForm(Path<? extends AcmIhmForm> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm ihm form.
	 *
	 * @param metadata the metadata
	 */
	public QAcmIhmForm(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm ihm form.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmIhmForm(PathMetadata metadata, PathInits inits) {

		this(AcmIhmForm.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm ihm form.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmIhmForm(Class<? extends AcmIhmForm> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.habilitationIHMRoute = inits.isInitialized("habilitationIHMRoute")
				? new QHabilitationIHMRoute(forProperty("habilitationIHMRoute"))
				: null;
	}

}
