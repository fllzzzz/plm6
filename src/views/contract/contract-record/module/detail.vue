<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="文件详情"
    :wrapper-closable="false"
    size="900px"
    custom-class="contract-change"
  >
    <template #content>
      <common-table
        ref="tableRef"
        :data="currentInfo"
        :max-height="maxHeight"
        style="width: 100%"
        return-source-data
        :showEmptySymbol="false"
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="sourceName" prop="sourceName" :show-overflow-tooltip="true" label="文件源" width="140px">
        <template v-slot="scope">
          <span>{{ scope.row.sourceName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="文件名" min-width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column key="createTime" prop="createTime" :show-overflow-tooltip="true" label="上传时间" min-width="150px">
        <template v-slot="scope">
          <span>{{ scope.row.createTime?parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="createUserName" prop="createUserName" :show-overflow-tooltip="true" label="操作人" min-width="150px">
        <template v-slot="scope">
          <span>{{ scope.row.createUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="filePath" prop="filePath" :show-overflow-tooltip="true" label="附件" min-width="150px">
        <template v-slot="scope">
          <export-button :params="{id: scope.row.id}" v-permission="permission.download"/>
        </template>
      </el-table-column>
    </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, inject, ref } from 'vue'

import { parseTime } from '@/utils/date'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'

import ExportButton from '@comp-common/export-button/index.vue'

const permission = inject('permission')
const props = defineProps({
  currentInfo: {
    type: Array,
    default: () => []
  },
  modelValue: {
    type: Boolean,
    require: true
  }
})

const drawerRef = ref()
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.contract-change',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body'
  },
  visible
)
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
