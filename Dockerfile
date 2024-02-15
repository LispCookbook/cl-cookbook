FROM ruby:2.5

COPY . /cl-cookbook
WORKDIR /cl-cookbook

RUN gem install bundler -v 2.1.2
# This spawns warnings..., let's ignore them using || true trick
RUN bundle install || true 

EXPOSE 4000

CMD ["bundle", "exec", "jekyll", "serve", "--host=0.0.0.0"]

# HOWTO RUN
# docker build -t cl-cookbook .
# docker run -p 4000:4000 -v $(pwd):/cl-cookbook cl-cookbook

