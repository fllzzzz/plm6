<template>
  <span class="ud-operation" style="display: inline-block">
    <common-button
      v-if="props.showDetail"
      v-permission="permission.get"
      :disabled="props.disabledDetail"
      size="mini"
      :type="props.detailType"
      :icon="props.detailIcon"
      @click.stop="toDetail(currentData)"
    />
    <common-button
      v-if="props.showEdit"
      v-permission="permission.edit"
      :disabled="props.disabledEdit"
      size="mini"
      :type="props.editType"
      icon="el-icon-edit"
      @click.stop="toEdit(currentData)"
    />
    <el-popover
      v-if="props.showDel && checkPermission(permission.del)"
      v-model:visible="pop"
      placement="top"
      width="180"
      trigger="manual"
      @show="onPopoverShow"
      @hide="onPopoverHide"
    >
      <p>{{ props.delPrompt }}</p>
      <div style="text-align: right; margin: 0">
        <common-button size="mini" type="text" @click="cancelDelete">取消</common-button>
        <common-button type="primary" size="mini" @click="handleDelete">确定</common-button>
      </div>
      <template #reference>
        <common-button
          :loading="crud.dataStatus[data.id].delete === 2"
          :disabled="props.disabledDel"
          type="danger"
          icon="el-icon-delete"
          size="mini"
          @click.stop="toDelete"
        />
      </template>
    </el-popover>
  </span>
</template>

<script setup>
import { computed, defineProps, ref, inject } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import { regExtra } from '@compos/use-crud'

const props = defineProps({
  data: {
    type: Object,
    required: true
  },
  disabledDetail: {
    type: Boolean,
    default: false
  },
  disabledEdit: {
    type: Boolean,
    default: false
  },
  disabledDel: {
    type: Boolean,
    default: false
  },
  showDetail: {
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
  editType: {
    type: String,
    default: 'primary'
  },
  detailType: {
    type: String,
    default: 'info'
  },
  detailIcon: {
    type: String,
    default: 'el-icon-view'
  },
  delPrompt: {
    type: String,
    default: '确定删除本条数据吗？'
  },
  beforeToDetail: {
    // 打开详情之前
    type: Function
  },
  beforeToEdit: {
    // 打开编辑之前
    type: Function
  }
})

const permission = inject('permission')

const pop = ref(false)

const { crud } = regExtra()

// 获取数据源
const currentData = computed(() => {
  const data = props.data
  if (data) {
    if (data.sourceRow) {
      return data.sourceRow
    } else {
      return data
    }
  }
  return data
})

// 取消删除
function cancelDelete() {
  pop.value = false
  crud.cancelDelete(currentData.value)
}

// 点击删除按钮
function toDelete() {
  pop.value = true
}

// 确认删除
function handleDelete() {
  pop.value = false
  crud.doDelete(currentData.value)
}

// 打开删除提示窗
function onPopoverShow() {
  setTimeout(() => {
    document.addEventListener('click', handleDocumentClick, { passive: false })
  }, 0)
}

// 隐藏删除提示窗
function onPopoverHide() {
  document.removeEventListener('click', handleDocumentClick)
}

// 打开详情
function toDetail(data) {
  if (typeof props.beforeToDetail === 'function') {
    props.beforeToDetail(data)
  }
  setTimeout(() => {
    crud.toDetail(data)
  }, 0)
}

// 打开编辑
function toEdit(data) {
  if (typeof props.beforeToEdit === 'function') {
    props.beforeToEdit(data)
  }
  setTimeout(() => {
    crud.toEdit(data)
  }, 0)
}

function handleDocumentClick(event) {
  pop.value = false
}
</script>

<style lang="scss" scoped>
::v-global(.ud-operation + .el-button) {
  margin-left: 7px;
}
::v-global(.el-button + .ud-operation) {
  margin-left: 7px;
}
.el-button + .el-button {
  margin-left: 7px;
}
</style>
