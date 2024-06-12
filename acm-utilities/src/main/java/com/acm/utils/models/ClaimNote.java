/*
 * 
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

// TODO: Auto-generated Javadoc
/**
 * The Class ClaimNote.
 */
@Entity
@Table(name = "ACM_CLAIM_NOTE")
public class ClaimNote extends GenericModel implements Serializable {

	
	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2622575264673979958L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;
	
	/** The comment. */
	@Column(name = "COMMENT")
	private String comment;
	
	/** The claim id. */
	@Column(name = "ID_CLAIM_NOTE")
	private Long claimId;

	/** The visibility. */
	@Column(name = "VISIBILITY")
	private String visibility;

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
	
	
}
