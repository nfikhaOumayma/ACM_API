/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.FileNotFoundException;
import java.util.List;

import org.apache.chemistry.opencmis.client.api.Document;
import org.springframework.web.multipart.MultipartFile;

import com.acm.constants.common.CommonConstantGED;
import com.acm.exceptions.type.GEDException;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.GedDocumentDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.models.AcmDocumentsGed;

/**
 * {@link DocumentService} class.Methodes relatives to GED manipulation.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public interface DocumentService {

	/**
	 * Upload to ged.
	 * 
	 * @author HaythemBenizid
	 * @param fileParameters the file parameters
	 * @return the list of uploaded documents ids
	 * @throws GEDException GEDException on error
	 * @throws FileNotFoundException FileNotFoundException on error
	 */
	List<String> uploadToGed(GedParameterDTO fileParameters)
			throws GEDException, FileNotFoundException;

	/**
	 * Upload list of files to ged.
	 *
	 * @author HaythemBenizid
	 * @param gedParameterDTOs the ged parameter DT os
	 * @return the list of uploaded documents ids
	 * @throws GEDException GEDException on error
	 * @throws FileNotFoundException FileNotFoundException on error
	 */
	List<GedParameterDTO> uploadListToGed(List<GedParameterDTO> gedParameterDTOs)
			throws GEDException, FileNotFoundException;

	/**
	 * standard method to upload files to GED.
	 * 
	 * @author HaythemBenizid
	 * @param files the list of files to be uploaded to GED
	 * @param tags the tags of files
	 * @param path the path where we want to save files in the GED server
	 * @return return the list of documents {@link Document} uploaded
	 * @throws GEDException on error
	 * @throws FileNotFoundException the file not found exception
	 */
	List<Document> upload(List<MultipartFile> files, List<String> tags, String path)
			throws GEDException, FileNotFoundException;

	/**
	 * Find all documents by given tag.
	 *
	 * @author HaythemBenizid
	 * @param tag the tag
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	List<GedDocumentDTO> findByTag(String tag) throws GEDException;

	/**
	 * Find all (if all set to TRUE else returning first {@link CommonConstantGED}.MAX_ITEMS_VALUE
	 * docs) documents by given {@link List} of tags.
	 *
	 * @author HaythemBenizid
	 * @param all the all
	 * @param tags the tags
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	List<GedDocumentDTO> find(Boolean all, List<String> tags) throws GEDException;

	/**
	 * Find the document by ID.
	 * 
	 * @author HaythemBenizid
	 * @param id the document id
	 * @return the requested document infomattion and his content in Byte[]
	 * @throws GEDException on error
	 */
	GedDocumentDTO find(String id) throws GEDException;

	/**
	 * Display document.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the byte[]
	 * @throws GEDException the GED exception
	 */
	byte[] displayDocument(String id) throws GEDException;

	/**
	 * download document.
	 *
	 * @author HaythemBenizid
	 * @param id the id document
	 * @return {@link Document} the type document
	 * @throws GEDException the GED exception
	 */
	Document downloadDocument(String id) throws GEDException;

	/**
	 * check if document exist in GED using ID document.
	 * 
	 * @author HaythemBenizid
	 * @param idDocumentGED the id document to be checked
	 * @return true when document exist/ false when document not exist
	 */
	Boolean isExist(String idDocumentGED);

	/**
	 * Checks if is exist in GED by ID & check if exist in backup Table {@link AcmDocumentsGed} by
	 * ID Document ACM .
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acm documents DTO
	 * @return the boolean
	 */
	Boolean isExist(AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * Delete document from GED.
	 * 
	 * @author HaythemBenizid
	 * @param idDocument the idDocument to be deleted
	 * @return true when deleted success else throw an exception
	 * @throws GEDException on error
	 */
	Boolean delete(String idDocument) throws GEDException;

	/**
	 * Find image for customer.
	 *
	 * @author ManelLamloum
	 * @param tag the tag
	 * @return the ged document DTO
	 * @throws GEDException the GED exception
	 */
	GedDocumentDTO findImageForCustomer(String tag) throws GEDException;
}
