/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.ged.factory;

import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.Session;

/**
 * {@link GedServiceFactory} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
public interface GedServiceFactory {

	/**
	 * Creates a new GedSession object.
	 * 
	 * @author HaythemBenizid
	 * @return the session
	 */
	Session createSession();

	/**
	 * Find ged site token.
	 * 
	 * @author HaythemBenizid
	 * @return the string
	 */
	String findGedSiteToken();

	/**
	 * Find site by given session.
	 * 
	 * @author HaythemBenizid
	 * @param session the session
	 * @return the folder
	 */
	Folder findSite(Session session);

	/**
	 * Adds the tag.
	 * 
	 * @author HaythemBenizid
	 * @param document the document
	 * @param tag the tag
	 * @param token the token
	 */
	void addTag(Document document, String tag, String token);

	/**
	 * Creates a new GedService object.
	 *
	 * @author HaythemBenizid
	 * @param session the session
	 * @param rootFolder the root folder
	 * @param folderName the folder name
	 * @return the folder
	 */
	Folder createOrLoadFolder(Session session, Folder rootFolder, String folderName);

}
