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
 * QSettingClaims is a Querydsl query type for SettingClaims.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingClaims extends EntityPathBase<SettingClaims> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1552669680L;

	/** The Constant settingClaims. */
	public static final QSettingClaims settingClaims = new QSettingClaims("settingClaims");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The assignement. */
	public final StringPath assignement = createString("assignement");

	/** The category. */
	public final StringPath category = createString("category");

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

	/** The pripority. */
	public final StringPath pripority = createString("pripority");

	/** The processing time line. */
	public final NumberPath<Integer> processingTimeLine =
			createNumber("processingTimeLine", Integer.class);

	/** The subject. */
	public final StringPath subject = createString("subject");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q setting claims.
	 *
	 * @param variable the variable
	 */
	public QSettingClaims(String variable) {

		super(SettingClaims.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting claims.
	 *
	 * @param path the path
	 */
	public QSettingClaims(Path<? extends SettingClaims> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting claims.
	 *
	 * @param metadata the metadata
	 */
	public QSettingClaims(PathMetadata metadata) {

		super(SettingClaims.class, metadata);
	}

}
