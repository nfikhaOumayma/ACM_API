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
 * QAcmIhmField is a Querydsl query type for AcmIhmField.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmIhmField extends EntityPathBase<AcmIhmField> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -13966304L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmIhmField. */
	public static final QAcmIhmField acmIhmField = new QAcmIhmField("acmIhmField");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm ihm field groupes. */
	public final SetPath<AcmIhmFieldGroupe, QAcmIhmFieldGroupe> acmIhmFieldGroupes =
			this.<AcmIhmFieldGroupe, QAcmIhmFieldGroupe>createSet("acmIhmFieldGroupes",
					AcmIhmFieldGroupe.class, QAcmIhmFieldGroupe.class, PathInits.DIRECT2);

	/** The acm ihm form. */
	public final QAcmIhmForm acmIhmForm;

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The code field. */
	public final StringPath codeField = createString("codeField");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The default value. */
	public final StringPath defaultValue = createString("defaultValue");

	/** The description. */
	public final StringPath description = createString("description");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The form control name. */
	public final StringPath formControlName = createString("formControlName");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The max. */
	public final NumberPath<Integer> max = createNumber("max", Integer.class);

	/** The min. */
	public final NumberPath<Integer> min = createNumber("min", Integer.class);

	/** The ordre. */
	public final NumberPath<Integer> ordre = createNumber("ordre", Integer.class);

	/** The placeholder. */
	public final StringPath placeholder = createString("placeholder");

	/** The single select. */
	public final BooleanPath singleSelect = createBoolean("singleSelect");

	/** The step. */
	public final NumberPath<Integer> step = createNumber("step", Integer.class);

	/** The sub code field. */
	public final StringPath subCodeField = createString("subCodeField");

	/** The titre. */
	public final StringPath titre = createString("titre");

	/** The type field. */
	public final StringPath typeField = createString("typeField");

	/** The type option values. */
	public final StringPath typeOptionValues = createString("typeOptionValues");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The validators. */
	public final SetPath<AcmIhmValidator, QAcmIhmValidator> validators =
			this.<AcmIhmValidator, QAcmIhmValidator>createSet("validators", AcmIhmValidator.class,
					QAcmIhmValidator.class, PathInits.DIRECT2);

	/**
	 * Instantiates a new q acm ihm field.
	 *
	 * @param variable the variable
	 */
	public QAcmIhmField(String variable) {

		this(AcmIhmField.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm ihm field.
	 *
	 * @param path the path
	 */
	public QAcmIhmField(Path<? extends AcmIhmField> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm ihm field.
	 *
	 * @param metadata the metadata
	 */
	public QAcmIhmField(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm ihm field.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmIhmField(PathMetadata metadata, PathInits inits) {

		this(AcmIhmField.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm ihm field.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmIhmField(Class<? extends AcmIhmField> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.acmIhmForm = inits.isInitialized("acmIhmForm")
				? new QAcmIhmForm(forProperty("acmIhmForm"), inits.get("acmIhmForm"))
				: null;
	}

}
