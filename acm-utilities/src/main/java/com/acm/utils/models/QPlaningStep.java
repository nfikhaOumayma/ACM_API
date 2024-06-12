package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QPlaningStep is a Querydsl query type for PlaningStep.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QPlaningStep extends EntityPathBase<PlaningStep> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1746996594L;

	/** The Constant planingStep. */
	public static final QPlaningStep planingStep = new QPlaningStep("planingStep");

	/** The begin or end month. */
	public final StringPath beginOrEndMonth = createString("beginOrEndMonth");

	/** The category. */
	public final StringPath category = createString("category");

	/** The date of month. */
	public final NumberPath<Integer> dateOfMonth = createNumber("dateOfMonth", Integer.class);

	/** The date of rep. */
	public final DateTimePath<java.util.Date> dateOfRep =
			createDateTime("dateOfRep", java.util.Date.class);

	/** The day rep. */
	public final NumberPath<Integer> dayRep = createNumber("dayRep", Integer.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The lst day. */
	public final StringPath lstDay = createString("lstDay");

	/** The month. */
	public final StringPath month = createString("month");

	/** The nbr month. */
	public final StringPath nbrMonth = createString("nbrMonth");

	/** The selected day. */
	public final StringPath selectedDay = createString("selectedDay");

	/**
	 * Instantiates a new q planing step.
	 *
	 * @param variable the variable
	 */
	public QPlaningStep(String variable) {

		super(PlaningStep.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q planing step.
	 *
	 * @param path the path
	 */
	public QPlaningStep(Path<? extends PlaningStep> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q planing step.
	 *
	 * @param metadata the metadata
	 */
	public QPlaningStep(PathMetadata metadata) {

		super(PlaningStep.class, metadata);
	}

}
