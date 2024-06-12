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
 * QReportSearchHistory is a Querydsl query type for ReportSearchHistory.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QReportSearchHistory extends EntityPathBase<ReportSearchHistory> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1660609535L;

	/** The Constant reportSearchHistory. */
	public static final QReportSearchHistory reportSearchHistory =
			new QReportSearchHistory("reportSearchHistory");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

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

	/** The label search history. */
	public final StringPath labelSearchHistory = createString("labelSearchHistory");

	/** The report name. */
	public final StringPath reportName = createString("reportName");

	/** The search data json. */
	public final StringPath searchDataJson = createString("searchDataJson");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The username. */
	public final StringPath username = createString("username");

	/**
	 * Instantiates a new q report search history.
	 *
	 * @param variable the variable
	 */
	public QReportSearchHistory(String variable) {

		super(ReportSearchHistory.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q report search history.
	 *
	 * @param path the path
	 */
	public QReportSearchHistory(Path<? extends ReportSearchHistory> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q report search history.
	 *
	 * @param metadata the metadata
	 */
	public QReportSearchHistory(PathMetadata metadata) {

		super(ReportSearchHistory.class, metadata);
	}

}
