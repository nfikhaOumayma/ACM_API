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
 * QSettingListValues is a Querydsl query type for SettingListValues.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingListValues extends EntityPathBase<SettingListValues> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2144568089L;

	/** The Constant settingListValues. */
	public static final QSettingListValues settingListValues =
			new QSettingListValues("settingListValues");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The date insertion. */
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id extern. */
	public final StringPath idExtern = createString("idExtern");

	/** The insert by. */
	public final StringPath insertBy = _super.insertBy;

	/** The list name. */
	public final StringPath listName = createString("listName");

	/** The parent id. */
	public final NumberPath<Long> parentId = createNumber("parentId", Long.class);

	/** The table abacus name. */
	public final StringPath tableAbacusName = createString("tableAbacusName");

	/** The updated by. */
	public final StringPath updatedBy = _super.updatedBy;

	/** The value json. */
	public final StringPath valueJson = createString("valueJson");

	/** The work flow. */
	public final SetPath<WorkFlowStep, QWorkFlowStep> workFlow =
			this.<WorkFlowStep, QWorkFlowStep>createSet("workFlow", WorkFlowStep.class,
					QWorkFlowStep.class, PathInits.DIRECT2);

	/**
	 * Instantiates a new q setting list values.
	 *
	 * @param variable the variable
	 */
	public QSettingListValues(String variable) {

		super(SettingListValues.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting list values.
	 *
	 * @param path the path
	 */
	public QSettingListValues(Path<? extends SettingListValues> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting list values.
	 *
	 * @param metadata the metadata
	 */
	public QSettingListValues(PathMetadata metadata) {

		super(SettingListValues.class, metadata);
	}

}
