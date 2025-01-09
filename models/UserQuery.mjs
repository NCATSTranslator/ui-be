export { UserQueryData };

class UserQueryData {
  constructor(description) {
    if (!description) throw new Error('description is required');
    this.description = description;
    this.note_ids = [];
    this.bookmark_ids = [];
    this.tags = [];
  }
}
