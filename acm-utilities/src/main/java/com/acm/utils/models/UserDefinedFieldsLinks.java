/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.acm.utils.audit.AuditTrailListener;

/**
 * {@link UserDefinedFieldsLinks} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Entity
@Table(name = "ACM_UDF_LINK")
@EntityListeners(AuditTrailListener.class)
public class UserDefinedFieldsLinks extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6081375115266148299L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_UDF_LINK", unique = true, nullable = false)
	private Long id;

	/** The user defined fields. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_UDF_FIELD")
	private UserDefinedFields userDefinedFields;

	/** The udf list value id. */
	@Column(name = "ID_ABACUS_FIELD_LIST_VALUE")
	private Long udfListValueId;

	/** The field value. */
	@Column(name = "FIELD_VALUE")
	private String fieldValue;

	/** The id abacus UDF link. */
	@Column(name = "ID_ABACUS_UDF_LINK")
	private Long idAbacusUDFLink;

	/** The surveys id. */
	@Column(name = "SURVEYS_ID_EXTERN")
	private Long surveysId;

	/** The index group. */
	@Column(name = "INDEX_GROUPE")
	private Long indexGroup;

	/** The cu account id. */
	@Transient
	private Long cuAccountId;

	/** The category. */
	@Column(name = "CATEGORY")
	private String category;

	/** The element id. */
	@Column(name = "ELEMENT_ID")
	private Long elementId;

	/** The token. */
	@Transient
	private String token;

	/**
	 * Instantiates a new user defined fields links.
	 */
	public UserDefinedFieldsLinks() {

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
	 * Gets the user defined fields.
	 *
	 * @return the userDefinedFields
	 */
	public UserDefinedFields getUserDefinedFields() {

		return userDefinedFields;
	}

	/**
	 * Sets the user defined fields.
	 *
	 * @param userDefinedFields the userDefinedFields to set
	 */
	public void setUserDefinedFields(UserDefinedFields userDefinedFields) {

		this.userDefinedFields = userDefinedFields;
	}

	/**
	 * Gets the udf list value id.
	 *
	 * @return the udfListValueId
	 */
	public Long getUdfListValueId() {

		return udfListValueId;
	}

	/**
	 * Sets the udf list value id.
	 *
	 * @param udfListValueId the udfListValueId to set
	 */
	public void setUdfListValueId(Long udfListValueId) {

		this.udfListValueId = udfListValueId;
	}

	/**
	 * Gets the field value.
	 *
	 * @return the fieldValue
	 */
	public String getFieldValue() {

		return fieldValue;
	}

	/**
	 * Sets the field value.
	 *
	 * @param fieldValue the fieldValue to set
	 */
	public void setFieldValue(String fieldValue) {

		this.fieldValue = fieldValue;
	}

	/**
	 * Gets the id abacus UDF link.
	 *
	 * @return the idAbacusUDFLink
	 */
	public Long getIdAbacusUDFLink() {

		return idAbacusUDFLink;
	}

	/**
	 * Sets the id abacus UDF link.
	 *
	 * @param idAbacusUDFLink the idAbacusUDFLink to set
	 */
	public void setIdAbacusUDFLink(Long idAbacusUDFLink) {

		this.idAbacusUDFLink = idAbacusUDFLink;
	}

	/**
	 * Gets the cu account id.
	 *
	 * @return the cuAccountId
	 */
	public Long getCuAccountId() {

		return cuAccountId;
	}

	/**
	 * Sets the cu account id.
	 *
	 * @param cuAccountId the cuAccountId to set
	 */
	public void setCuAccountId(Long cuAccountId) {

		this.cuAccountId = cuAccountId;
	}

	/**
	 * Gets the token.
	 *
	 * @return the token
	 */
	public String getToken() {

		return token;
	}

	/**
	 * Sets the token.
	 *
	 * @param token the token to set
	 */
	public void setToken(String token) {

		this.token = token;
	}

	/**
	 * Gets the surveys id.
	 *
	 * @return the surveysId
	 */
	public Long getSurveysId() {

		return surveysId;
	}

	/**
	 * Sets the surveys id.
	 *
	 * @param surveysId the surveysId to set
	 */
	public void setSurveysId(Long surveysId) {

		this.surveysId = surveysId;
	}

	/**
	 * Gets the index group.
	 *
	 * @return the index group
	 */
	public Long getIndexGroup() {

		return indexGroup;
	}

	/**
	 * Sets the index group.
	 *
	 * @param indexGroup the new index group
	 */
	public void setIndexGroup(Long indexGroup) {

		this.indexGroup = indexGroup;
	}

	/**
	 * Gets the category.
	 *
	 * @return the category
	 */
	public String getCategory() {

		return category;
	}

	/**
	 * Sets the category.
	 *
	 * @param category the new category
	 */
	public void setCategory(String category) {

		this.category = category;
	}

	/**
	 * Gets the element id.
	 *
	 * @return the element id
	 */
	public Long getElementId() {

		return elementId;
	}

	/**
	 * Sets the element id.
	 *
	 * @param elementId the new element id
	 */
	public void setElementId(Long elementId) {

		this.elementId = elementId;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "UserDefinedFieldsLinks [id=" + id + ", userDefinedFields=" + userDefinedFields
				+ ", udfListValueId=" + udfListValueId + ", fieldValue=" + fieldValue
				+ ", idAbacusUDFLink=" + idAbacusUDFLink + ", surveysId=" + surveysId
				+ ", cuAccountId=" + cuAccountId + "]";
	}
}
