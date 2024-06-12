/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.Date;

/**
 * {@link AcmHistoryDTO} class.
 *
 * @author hchaouachi
 * @since 0.1.0
 */
public class AcmHistoryDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3868475557529653180L;

	/** The id. */
	private Long id;

	/** The id object. */
	private Long idObject;

	/** The actions. */
	private String actions;

	/** The value object. */
	private String valueObject;

	/** The date action. */
	private Date dateAction;

	/** The action by. */
	private String actionBy;

	/** The type object. */
	private String typeObject;

	/** The object. */
	private Object object;

	/**
	 * Instantiates a new acm history DTO.
	 */
	public AcmHistoryDTO() {

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the actions.
	 *
	 * @return the actions
	 */
	public String getActions() {

		return actions;
	}

	/**
	 * Sets the actions.
	 *
	 * @param actions the new actions
	 */
	public void setActions(String actions) {

		this.actions = actions;
	}

	/**
	 * Gets the id object.
	 *
	 * @return the id object
	 */
	public Long getIdObject() {

		return idObject;
	}

	/**
	 * Sets the id object.
	 *
	 * @param idObject the new id object
	 */
	public void setIdObject(Long idObject) {

		this.idObject = idObject;
	}

	/**
	 * Gets the value object.
	 *
	 * @return the value object
	 */
	public String getValueObject() {

		return valueObject;
	}

	/**
	 * Sets the value object.
	 *
	 * @param valueObject the new value object
	 */
	public void setValueObject(String valueObject) {

		this.valueObject = valueObject;
	}

	/**
	 * Gets the date action.
	 *
	 * @return the date action
	 */
	public Date getDateAction() {

		return dateAction;
	}

	/**
	 * Sets the date action.
	 *
	 * @param dateAction the new date action
	 */
	public void setDateAction(Date dateAction) {

		this.dateAction = dateAction;
	}

	/**
	 * Gets the action by.
	 *
	 * @return the action by
	 */
	public String getActionBy() {

		return actionBy;
	}

	/**
	 * Sets the action by.
	 *
	 * @param actionBy the new action by
	 */
	public void setActionBy(String actionBy) {

		this.actionBy = actionBy;
	}

	/**
	 * Gets the type object.
	 *
	 * @return the type object
	 */
	public String getTypeObject() {

		return typeObject;
	}

	/**
	 * Sets the type object.
	 *
	 * @param typeObject the new type object
	 */
	public void setTypeObject(String typeObject) {

		this.typeObject = typeObject;
	}

	/**
	 * Gets the object.
	 *
	 * @return the object
	 */
	public Object getObject() {

		return object;
	}

	/**
	 * Sets the object.
	 *
	 * @param object the new object
	 */
	public void setObject(Object object) {

		this.object = object;
	}

}
