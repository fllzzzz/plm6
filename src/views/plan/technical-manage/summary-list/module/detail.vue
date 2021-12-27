<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="区域详情"
    :wrapper-closable="false"
    size="900px"
  >
    <template #title>
      <div class="dialog-title">
        <span style="margin-right: 5px">区域详情</span>
        <span style="position: absolute; right: 20px">
          <common-button size="small" @click="handleClose">关闭</common-button>
        </span>
      </div>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        :data="[{id:1}]"
        style="width: 100%"
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="区域" width="140px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="fileName" prop="fileName" :show-overflow-tooltip="true" label="轴线/标高" min-width="160px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="fileName" prop="fileName" :show-overflow-tooltip="true" label="合计数量" min-width="150px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="fileName" prop="fileName" :show-overflow-tooltip="true" label="合计毛重(kg)" min-width="150px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <el-table-column key="fileName" prop="fileName" :show-overflow-tooltip="true" label="合计净重(kg)" min-width="150px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
    </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'
import useVisible from '@compos/use-visible'

const props = defineProps({
  currentInfo: {
    type: Array,
    default: () => []
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  type: {
    type: String,
    require: true
  }
})

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
