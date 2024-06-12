/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link CollectionNote} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Entity
@Table(name = "ACM_COLLECTION_NOTE")
public class CollectionNote extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 240944917002635921L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

	/** The action. */
	@Column(name = "ACTION")
	private String action;

	/** The comment. */
	@Column(name = "COMMENT")
	private String comment;

	/** The acm loan id. */
	@Column(name = "COLLECTION_ID")
	private Long collectionId;

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
	 * Gets the action.
	 *
	 * @return the action
	 */
	public String getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the new action
	 */
	public void setAction(String action) {

		this.action = action;
	}

	/**
	 * Gets the comment.
	 *
	 * @return the comment
	 */
	public String getComment() {

		return comment;
	}

	/**
	 * Sets the comment.
	 *
	 * @param comment the new comment
	 */
	public void setComment(String comment) {

		this.comment = comment;
	}

	/**
	 * Gets the collection id.
	 *
	 * @return the collection id
	 */
	public Long getCollectionId() {

		return collectionId;
	}

	/**
	 * Sets the collection id.
	 *
	 * @param collectionId the new collection id
	 */
	public void setCollectionId(Long collectionId) {

		this.collectionId = collectionId;
	}

}
