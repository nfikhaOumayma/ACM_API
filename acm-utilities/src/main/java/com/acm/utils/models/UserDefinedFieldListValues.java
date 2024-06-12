/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import com.acm.utils.audit.AuditTrailListener;

/**
 * {@link UserDefinedFieldListValues} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_UDF_LIST_VALUES")
@EntityListeners(AuditTrailListener.class)
public class UserDefinedFieldListValues extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 8864771102849586716L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_UDF_LIST_VALUES", unique = true, nullable = false)
	private Long id;

	/** The table abacus name. */
	@Column(name = "TABLE_ABACUS_NAME", length = 512)
	private String tableAbacusName;

	/** The id UDF list. */
	@Column(name = "ID_UDF_LIST")
	private Long idUDFList;

	/** The id UDF list value. */
	@Column(name = "ID_UDF_LIST_VALUE")
	private Long idUDFListValue;

	/** The id UDF list link. */
	@Column(name = "ID_UDF_LIST_LINK")
	private Long idUDFListLink;

	/** The score. */
	@Column(name = "SCORE")
	private Integer score;

	/** The name. */
	@Column(name = "NAME")
	private String name;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The parent UDF list value. */
	@Column(name = "PARENT_UDF_LIST_VALUE")
	private Long parentUDFListValue;

	/**
	 * Instantiates a new user defined field list values.
	 */
	public UserDefinedFieldListValues() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the table abacus name.
	 *
	 * @return the tableAbacusName
	 */
	public String getTableAbacusName() {

		return tableAbacusName;
	}

	/**
	 * Sets the table abacus name.
	 *
	 * @param tableAbacusName the tableAbacusName to set
	 */
	public void setTableAbacusName(String tableAbacusName) {

		this.tableAbacusName = tableAbacusName;
	}

	/**
	 * Gets the id UDF list.
	 *
	 * @return the idUDFList
	 */
	public Long getIdUDFList() {

		return idUDFList;
	}

	/**
	 * Sets the id UDF list.
	 *
	 * @param idUDFList the idUDFList to set
	 */
	public void setIdUDFList(Long idUDFList) {

		this.idUDFList = idUDFList;
	}

	/**
	 * Gets the id UDF list value.
	 *
	 * @return the idUDFListValue
	 */
	public Long getIdUDFListValue() {

		return idUDFListValue;
	}

	/**
	 * Sets the id UDF list value.
	 *
	 * @param idUDFListValue the idUDFListValue to set
	 */
	public void setIdUDFListValue(Long idUDFListValue) {

		this.idUDFListValue = idUDFListValue;
	}

	/**
	 * Gets the id UDF list link.
	 *
	 * @return the idUDFListLink
	 */
	public Long getIdUDFListLink() {

		return idUDFListLink;
	}

	/**
	 * Sets the id UDF list link.
	 *
	 * @param idUDFListLink the idUDFListLink to set
	 */
	public void setIdUDFListLink(Long idUDFListLink) {

		this.idUDFListLink = idUDFListLink;
	}

	/**
	 * Gets the score.
	 *
	 * @return the score
	 */
	public Integer getScore() {

		return score;
	}

	/**
	 * Sets the score.
	 *
	 * @param score the score to set
	 */
	public void setScore(Integer score) {

		this.score = score;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the parent UDF list value.
	 *
	 * @return the parentUDFListValue
	 */
	public Long getParentUDFListValue() {

		return parentUDFListValue;
	}

	/**
	 * Sets the parent UDF list value.
	 *
	 * @param parentUDFListValue the parentUDFListValue to set
	 */
	public void setParentUDFListValue(Long parentUDFListValue) {

		this.parentUDFListValue = parentUDFListValue;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UserDefinedFieldListValues [id=" + id + ", tableAbacusName=" + tableAbacusName
				+ ", idUDFList=" + idUDFList + ", idUDFListValue=" + idUDFListValue
				+ ", idUDFListLink=" + idUDFListLink + ", score=" + score + ", name=" + name
				+ ", description=" + description + "]";
	}

}
