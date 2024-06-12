/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.Date;

/**
 * {@link LoanAbacusAPIModelNotes} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class LoanAbacusAPIModelNotes implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1217967413905357610L;

	/** The note ID. */
	private int noteID;

	/** The note. */
	private String note;

	/** The user ID. */
	private int userID;

	/** The terminal ID. */
	private int terminalID;

	/** The creation date. */
	private Date creationDate;

	/** The edit date. */
	private Date editDate;

	/** The user name. */
	private String userName;

	/**
	 * Instantiates a new loan abacus API model notes.
	 */
	public LoanAbacusAPIModelNotes() {

		/*
		 * Empty
		 */
	}

	/**
	 * Gets the note ID.
	 *
	 * @return the noteID
	 */
	public int getNoteID() {

		return noteID;
	}

	/**
	 * Sets the note ID.
	 *
	 * @param noteID the noteID to set
	 */
	public void setNoteID(int noteID) {

		this.noteID = noteID;
	}

	/**
	 * Gets the note.
	 *
	 * @return the note
	 */
	public String getNote() {

		return note;
	}

	/**
	 * Sets the note.
	 *
	 * @param note the note to set
	 */
	public void setNote(String note) {

		this.note = note;
	}

	/**
	 * Gets the user ID.
	 *
	 * @return the userID
	 */
	public int getUserID() {

		return userID;
	}

	/**
	 * Sets the user ID.
	 *
	 * @param userID the userID to set
	 */
	public void setUserID(int userID) {

		this.userID = userID;
	}

	/**
	 * Gets the terminal ID.
	 *
	 * @return the terminalID
	 */
	public int getTerminalID() {

		return terminalID;
	}

	/**
	 * Sets the terminal ID.
	 *
	 * @param terminalID the terminalID to set
	 */
	public void setTerminalID(int terminalID) {

		this.terminalID = terminalID;
	}

	/**
	 * Gets the creation date.
	 *
	 * @return the creationDate
	 */
	public Date getCreationDate() {

		return creationDate;
	}

	/**
	 * Sets the creation date.
	 *
	 * @param creationDate the creationDate to set
	 */
	public void setCreationDate(Date creationDate) {

		this.creationDate = creationDate;
	}

	/**
	 * Gets the edits the date.
	 *
	 * @return the editDate
	 */
	public Date getEditDate() {

		return editDate;
	}

	/**
	 * Sets the edits the date.
	 *
	 * @param editDate the editDate to set
	 */
	public void setEditDate(Date editDate) {

		this.editDate = editDate;
	}

	/**
	 * Gets the user name.
	 *
	 * @return the userName
	 */
	public String getUserName() {

		return userName;
	}

	/**
	 * Sets the user name.
	 *
	 * @param userName the userName to set
	 */
	public void setUserName(String userName) {

		this.userName = userName;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "LoanAbacusAPIModelNotes [noteID=" + noteID + ", note=" + note + ", userID=" + userID
				+ ", terminalID=" + terminalID + ", creationDate=" + creationDate + ", editDate="
				+ editDate + ", userName=" + userName + "]";
	}

}
