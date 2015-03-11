from django.conf.urls import patterns, url

urlpatterns = patterns(
    '',
    url(r'^$', 'samsweb.views.home_page', name='home'),
    url(r'^run$', 'samsweb.views.run_model', name='run_model'),
    url(r'^model_configuration/$',
        'samsweb.views.get_model_configuration',
        name='get_model_configuration'),
    url(r'^images/(?P<image_class>[a-z_]+)/(?P<image_name>[0-9a-z_-]+)/$',
        'samsweb.views.get_image', name='get_image'),
)
