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
 * QReportVisit is a Querydsl query type for ReportVisit.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QReportVisit extends EntityPathBase<ReportVisit> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 284787840L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant reportVisit. */
	public static final QReportVisit reportVisit = new QReportVisit("reportVisit");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The comment. */
	public final StringPath comment = createString("comment");

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

	/** The id report visit. */
	public final NumberPath<Long> idReportVisit = createNumber("idReportVisit", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan. */
	public final QLoan loan;

	/** The planned visit. */
	public final DateTimePath<java.util.Date> plannedVisit =
			createDateTime("plannedVisit", java.util.Date.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The visit by. */
	public final StringPath visitBy = createString("visitBy");

	/**
	 * Instantiates a new q report visit.
	 *
	 * @param variable the variable
	 */
	public QReportVisit(String variable) {

		this(ReportVisit.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q report visit.
	 *
	 * @param path the path
	 */
	public QReportVisit(Path<? extends ReportVisit> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q report visit.
	 *
	 * @param metadata the metadata
	 */
	public QReportVisit(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q report visit.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QReportVisit(PathMetadata metadata, PathInits inits) {

		this(ReportVisit.class, metadata, inits);
	}

	/**
	 * Instantiates a new q report visit.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QReportVisit(Class<? extends ReportVisit> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}
