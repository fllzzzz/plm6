<template>
  <div style="display:inline-block">
    <el-button
      v-if="showEdit"
      v-permission="permission.edit"
      :disabled="disabledEdit"
      size="mini"
      :type="editBtnType"
      icon="el-icon-edit"
      @click.stop="crud.toEdit(data)"
    />
    <el-popover
      v-if="showDel"
      v-model="pop"
      v-permission="permission.del"
      placement="top"
      width="180"
      trigger="manual"
      @show="onPopoverShow"
      @hide="onPopoverHide"
    >
      <p>{{ delPrompt }}</p>
      <div style="text-align: right; margin: 0">
        <el-button size="mini" type="text" @click="doCancel">取消</el-button>
        <el-button :loading="crud.dataStatus[data.id].delete === 2" type="primary" size="mini" @click="crud.doDelete(data)">确定</el-button>
      </div>
      <template v-slot:reference>
        <el-button :disabled="disabledDle" type="danger" icon="el-icon-delete" size="mini" @click.stop="toDelete" />
      </template>
    </el-popover>
  </div>
</template>
<script>
import CRUD, { crud } from '@crud/crud'
export default {
  mixins: [crud()],
  inject: ['permission'],
  props: {
    data: {
      type: Object,
      required: true
    },
    disabledEdit: {
      type: Boolean,
      default: false
    },
    disabledDle: {
      type: Boolean,
      default: false
    },
    showDel: {
      type: Boolean,
      default: true
    },
    showEdit: {
      type: Boolean,
      default: true
    },
    editBtnType: {
      type: String,
      default: 'primary'
    },
    delPrompt: {
      type: String,
      default: '确定删除本条数据吗？'
    }
  },
  data() {
    return {
      pop: false
    }
  },
  methods: {
    doCancel() {
      this.pop = false
      this.crud.cancelDelete(this.data)
    },
    toDelete() {
      this.pop = true
    },
    [CRUD.HOOK.afterDelete](crud, data) {
      if (data === this.data) {
        this.pop = false
      }
    },
    onPopoverShow() {
      setTimeout(() => {
        document.addEventListener('click', this.handleDocumentClick, { passive: false })
      }, 0)
    },
    onPopoverHide() {
      document.removeEventListener('click', this.handleDocumentClick)
    },
    handleDocumentClick(event) {
      this.pop = false
    }
  }
}
</script>
