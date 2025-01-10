export { UserQueryData };

class UserQueryData {
  constructor(description) {
    if (!description) throw new Error('description is required');
    this.description = description;
    this.bookmark_ids = [];
    this.tag_ids = [];
  }
}
