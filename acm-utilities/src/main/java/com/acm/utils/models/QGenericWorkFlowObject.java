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
 * QGenericWorkFlowObject is a Querydsl query type for GenericWorkFlowObject.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QGenericWorkFlowObject extends EntityPathBase<GenericWorkFlowObject> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1276456574L;

	/** The Constant genericWorkFlowObject. */
	public static final QGenericWorkFlowObject genericWorkFlowObject =
			new QGenericWorkFlowObject("genericWorkFlowObject");

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

	/** The generic work flow objects. */
	public final SetPath<Item, QItem> genericWorkFlowObjects = this.<Item, QItem>createSet(
			"genericWorkFlowObjects", Item.class, QItem.class, PathInits.DIRECT2);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The name. */
	public final StringPath name = createString("name");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q generic work flow object.
	 *
	 * @param variable the variable
	 */
	public QGenericWorkFlowObject(String variable) {

		super(GenericWorkFlowObject.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q generic work flow object.
	 *
	 * @param path the path
	 */
	public QGenericWorkFlowObject(Path<? extends GenericWorkFlowObject> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q generic work flow object.
	 *
	 * @param metadata the metadata
	 */
	public QGenericWorkFlowObject(PathMetadata metadata) {

		super(GenericWorkFlowObject.class, metadata);
	}

}
