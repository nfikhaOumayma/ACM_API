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
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;

/**
 * QWorkflowStepUdfGroupe is a Querydsl query type for WorkflowStepUdfGroupe.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QWorkflowStepUdfGroupe extends EntityPathBase<WorkflowStepUdfGroupe> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2041343547L;

	/** The Constant workflowStepUdfGroupe. */
	public static final QWorkflowStepUdfGroupe workflowStepUdfGroupe =
			new QWorkflowStepUdfGroupe("workflowStepUdfGroupe");

	/** The id collection step. */
	public final NumberPath<Long> idCollectionStep = createNumber("idCollectionStep", Long.class);

	/** The id user defined field group. */
	public final NumberPath<Long> idUserDefinedFieldGroup =
			createNumber("idUserDefinedFieldGroup", Long.class);

	/** The id user defined fields. */
	public final NumberPath<Long> idUserDefinedFields =
			createNumber("idUserDefinedFields", Long.class);

	/** The id workflow step. */
	public final NumberPath<Long> idWorkflowStep = createNumber("idWorkflowStep", Long.class);

	/** The id work flow step udf group. */
	public final NumberPath<Long> idWorkFlowStepUdfGroup =
			createNumber("idWorkFlowStepUdfGroup", Long.class);

	/** The mandatory. */
	public final BooleanPath mandatory = createBoolean("mandatory");

	/**
	 * Instantiates a new q workflow step udf groupe.
	 *
	 * @param variable the variable
	 */
	public QWorkflowStepUdfGroupe(String variable) {

		super(WorkflowStepUdfGroupe.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q workflow step udf groupe.
	 *
	 * @param path the path
	 */
	public QWorkflowStepUdfGroupe(Path<? extends WorkflowStepUdfGroupe> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q workflow step udf groupe.
	 *
	 * @param metadata the metadata
	 */
	public QWorkflowStepUdfGroupe(PathMetadata metadata) {

		super(WorkflowStepUdfGroupe.class, metadata);
	}

}
