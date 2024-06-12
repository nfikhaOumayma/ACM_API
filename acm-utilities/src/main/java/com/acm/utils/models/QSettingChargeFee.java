/*
 * 
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
 * QSettingChargeFee is a Querydsl query type for SettingChargeFee.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSettingChargeFee extends EntityPathBase<SettingChargeFee> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 655126873L;

	/** The Constant settingChargeFee. */
	public static final QSettingChargeFee settingChargeFee =
			new QSettingChargeFee("settingChargeFee");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The amount. */
	public final NumberPath<java.math.BigDecimal> amount =
			createNumber("amount", java.math.BigDecimal.class);

	/** The code. */
	public final StringPath code = createString("code");

	/** The cufee id. */
	public final NumberPath<Integer> cufeeId = createNumber("cufeeId", Integer.class);

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

	/** The id collection. */
	public final NumberPath<Long> idCollection = createNumber("idCollection", Long.class);

	/** The id loan extern. */
	public final NumberPath<Long> idLoanExtern = createNumber("idLoanExtern", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The label. */
	public final StringPath label = createString("label");

	/** The percentage. */
	public final NumberPath<Integer> percentage = createNumber("percentage", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The value. */
	public final StringPath value = createString("value");

	/**
	 * Instantiates a new q setting charge fee.
	 *
	 * @param variable the variable
	 */
	public QSettingChargeFee(String variable) {

		super(SettingChargeFee.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q setting charge fee.
	 *
	 * @param path the path
	 */
	public QSettingChargeFee(Path<? extends SettingChargeFee> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q setting charge fee.
	 *
	 * @param metadata the metadata
	 */
	public QSettingChargeFee(PathMetadata metadata) {

		super(SettingChargeFee.class, metadata);
	}

}
