/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.proxyservice.filter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.netflix.zuul.filters.support.FilterConstants;
import org.springframework.stereotype.Component;

import com.netflix.zuul.ZuulFilter;
import com.netflix.zuul.context.RequestContext;
import com.netflix.zuul.exception.ZuulException;

/**
 * {@link ReponseFilter} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Component
public class ReponseFilter extends ZuulFilter {

	/** The logger. */
	Logger logger = LoggerFactory.getLogger(this.getClass());

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.ZuulFilter#filterType()
	 */
	@Override
	public String filterType() {

		// 4 possibilités :
		// pre : permet d'exécuter du code avant la redirection de la requête vers sa destination
		// finale.
		// post : permet d'exécuter du code après que la requête a été redirigée.
		// route : permet d'agir sur la façon de rediriger les requêtes.
		// error : permet d'agir en cas d'erreur lors de la redirection de la requête.
		return FilterConstants.POST_TYPE;
	}

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.ZuulFilter#filterOrder()
	 */
	@Override
	public int filterOrder() {

		return 2;
	}

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.IZuulFilter#shouldFilter()
	 */
	@Override
	public boolean shouldFilter() {

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see com.netflix.zuul.IZuulFilter#run()
	 */
	@Override
	public Object run() throws ZuulException {

		HttpServletRequest request = RequestContext.getCurrentContext().getRequest();
		HttpServletResponse response = RequestContext.getCurrentContext().getResponse();
		logger.debug("**** Reponse CODE HTTP [{}] for url : [{}] ****", response.getStatus(),
				request.getRequestURL());

		if (response.getStatus() == 0) {
			response.setStatus(HttpStatus.SC_INTERNAL_SERVER_ERROR);
		}
		return null;
	}
}
