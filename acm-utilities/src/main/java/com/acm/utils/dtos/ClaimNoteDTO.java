/*
 * 
 */
package com.acm.utils.dtos;

import java.util.Date;

// TODO: Auto-generated Javadoc
/**
 * The Class ClaimNoteDTO.
 */
public class ClaimNoteDTO extends GenericDTO implements java.io.Serializable {
	

	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3117606932501521075L;

	/** The id. */
	private Long id;
	
	/** The comment. */
	private String comment;
	
	/** The claim id. */
	private Long claimId;
	
	/** The visibility. */
	private String visibility;

	/** The insert by. */
	private String insertBy;

	/** The date insertion. */
	private Date dateInsertion;
	
	/** The date last update. */
	private Date dateLastUpdate;

	/** The updated by. */
	private String updatedBy;

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
	 * Gets the claim id.
	 *
	 * @return the claim id
	 */
	public Long getClaimId() {
		return claimId;
	}

	/**
	 * Sets the claim id.
	 *
	 * @param claimId the new claim id
	 */
	public void setClaimId(Long claimId) {
		this.claimId = claimId;
	}

	/**
	 * Gets the visibility.
	 *
	 * @return the visibility
	 */
	public String getVisibility() {
		return visibility;
	}

	/**
	 * Sets the visibility.
	 *
	 * @param visibility the new visibility
	 */
	public void setVisibility(String visibility) {
		this.visibility = visibility;
	}

	/**
	 * Gets the insert by.
	 *
	 * @return the insert by
	 */
	public String getInsertBy() {
		return insertBy;
	}

	/**
	 * Sets the insert by.
	 *
	 * @param insertBy the new insert by
	 */
	public void setInsertBy(String insertBy) {
		this.insertBy = insertBy;
	}

	/**
	 * Gets the date insertion.
	 *
	 * @return the date insertion
	 */
	public Date getDateInsertion() {
		return dateInsertion;
	}

	/**
	 * Sets the date insertion.
	 *
	 * @param dateInsertion the new date insertion
	 */
	public void setDateInsertion(Date dateInsertion) {
		this.dateInsertion = dateInsertion;
	}

	/**
	 * Gets the date last update.
	 *
	 * @return the date last update
	 */
	public Date getDateLastUpdate() {
		return dateLastUpdate;
	}

	/**
	 * Sets the date last update.
	 *
	 * @param dateLastUpdate the new date last update
	 */
	public void setDateLastUpdate(Date dateLastUpdate) {
		this.dateLastUpdate = dateLastUpdate;
	}

	/**
	 * Gets the updated by.
	 *
	 * @return the updated by
	 */
	public String getUpdatedBy() {
		return updatedBy;
	}

	/**
	 * Sets the updated by.
	 *
	 * @param updatedBy the new updated by
	 */
	public void setUpdatedBy(String updatedBy) {
		this.updatedBy = updatedBy;
	}
	
	
}
