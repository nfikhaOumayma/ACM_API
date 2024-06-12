/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link GedDocumentDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public class GedDocumentDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 6280723646581049815L;

	/** The created at. */
	private String createdAt;

	/** The is folder. */
	private String isFolder;

	/** The is file. */
	private String isFile;

	/** The modified at. */
	private String modifiedAt;

	/** The name. */
	private String name;

	/** The id. */
	private String id;

	/** The mime type. */
	private String mimeType;

	/** The mime type name. */
	private String mimeTypeName;

	/** The parent id. */
	private String parentId;

	/** The categorie. */
	private String categorie;

	/** The parents. */
	private String parents;

	/** The document content byte. */
	private byte[] documentContentByte;

	/**
	 * Instantiates a new document DTO.
	 */
	public GedDocumentDTO() {

		/*
		 * 
		 */
	}

	/**
	 * Instantiates a new document DTO.
	 *
	 * @param createdAt the created at
	 * @param isFolder the is folder
	 * @param isFile the is file
	 * @param modifiedAt the modified at
	 * @param name the name
	 * @param id the id
	 * @param mimeType the mime type
	 * @param mimeTypeName the mime type name
	 * @param parentId the parent id
	 */
	public GedDocumentDTO(String createdAt, String isFolder, String isFile, String modifiedAt,
			String name, String id, String mimeType, String mimeTypeName, String parentId) {

		this.createdAt = createdAt;
		this.isFolder = isFolder;
		this.isFile = isFile;
		this.modifiedAt = modifiedAt;
		this.name = name;
		this.id = id;
		this.mimeType = mimeType;
		this.mimeTypeName = mimeTypeName;
		this.parentId = parentId;
	}

	/**
	 * Instantiates a new document DTO.
	 *
	 * @param createdAt the created at
	 * @param isFolder the is folder
	 * @param isFile the is file
	 * @param modifiedAt the modified at
	 * @param name the name
	 * @param id the id
	 * @param mimeType the mime type
	 * @param parentId the parent id
	 */
	public GedDocumentDTO(String createdAt, String isFolder, String isFile, String modifiedAt,
			String name, String id, String mimeType, String parentId) {

		this.createdAt = createdAt;
		this.isFolder = isFolder;
		this.isFile = isFile;
		this.modifiedAt = modifiedAt;
		this.name = name;
		this.id = id;
		this.mimeType = mimeType;
		this.parentId = parentId;
	}

	/**
	 * Gets the created at.
	 *
	 * @return the createdAt
	 */
	public String getCreatedAt() {

		return createdAt;
	}

	/**
	 * Sets the created at.
	 *
	 * @param createdAt the createdAt to set
	 */
	public void setCreatedAt(String createdAt) {

		this.createdAt = createdAt;
	}

	/**
	 * Gets the checks if is folder.
	 *
	 * @return the isFolder
	 */
	public String getIsFolder() {

		return isFolder;
	}

	/**
	 * Sets the checks if is folder.
	 *
	 * @param isFolder the isFolder to set
	 */
	public void setIsFolder(String isFolder) {

		this.isFolder = isFolder;
	}

	/**
	 * Gets the checks if is file.
	 *
	 * @return the isFile
	 */
	public String getIsFile() {

		return isFile;
	}

	/**
	 * Sets the checks if is file.
	 *
	 * @param isFile the isFile to set
	 */
	public void setIsFile(String isFile) {

		this.isFile = isFile;
	}

	/**
	 * Gets the modified at.
	 *
	 * @return the modifiedAt
	 */
	public String getModifiedAt() {

		return modifiedAt;
	}

	/**
	 * Sets the modified at.
	 *
	 * @param modifiedAt the modifiedAt to set
	 */
	public void setModifiedAt(String modifiedAt) {

		this.modifiedAt = modifiedAt;
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
	 * Gets the id.
	 *
	 * @return the id
	 */
	public String getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(String id) {

		this.id = id;
	}

	/**
	 * Gets the mime type.
	 *
	 * @return the mimeType
	 */
	public String getMimeType() {

		return mimeType;
	}

	/**
	 * Sets the mime type.
	 *
	 * @param mimeType the mimeType to set
	 */
	public void setMimeType(String mimeType) {

		this.mimeType = mimeType;
	}

	/**
	 * Gets the mime type name.
	 *
	 * @return the mimeTypeName
	 */
	public String getMimeTypeName() {

		return mimeTypeName;
	}

	/**
	 * Sets the mime type name.
	 *
	 * @param mimeTypeName the mimeTypeName to set
	 */
	public void setMimeTypeName(String mimeTypeName) {

		this.mimeTypeName = mimeTypeName;
	}

	/**
	 * Gets the parent id.
	 *
	 * @return the parentId
	 */
	public String getParentId() {

		return parentId;
	}

	/**
	 * Sets the parent id.
	 *
	 * @param parentId the parentId to set
	 */
	public void setParentId(String parentId) {

		this.parentId = parentId;
	}

	/**
	 * Gets the categorie.
	 *
	 * @return the categorie
	 */
	public String getCategorie() {

		return categorie;
	}

	/**
	 * Sets the categorie.
	 *
	 * @param categorie the categorie to set
	 */
	public void setCategorie(String categorie) {

		this.categorie = categorie;
	}

	/**
	 * Gets the parents.
	 *
	 * @return the parents
	 */
	public String getParents() {

		return parents;
	}

	/**
	 * Sets the parents.
	 *
	 * @param parents the parents to set
	 */
	public void setParents(String parents) {

		this.parents = parents;
	}

	/**
	 * Gets the document content byte.
	 *
	 * @return the documentContentByte
	 */
	public byte[] getDocumentContentByte() {

		return documentContentByte;
	}

	/**
	 * Sets the document content byte.
	 *
	 * @param documentContentByte the documentContentByte to set
	 */
	public void setDocumentContentByte(byte[] documentContentByte) {

		this.documentContentByte = documentContentByte;
	}

}
