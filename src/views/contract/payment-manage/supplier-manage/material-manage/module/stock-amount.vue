<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="入库记录"
    :wrapper-closable="false"
    size="80%"
  >
    <template #content>
      <common-table
        ref="tableRef"
        :data="[{id:1}]"
        :max-height="maxHeight"
        :showEmptySymbol="false"
        style="width: 100%"
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="入库日期">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="编号">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="名称">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="种类">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="规格">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="计量单位">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="入库数">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="核算单位">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="入库量">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="含税单价">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="含税金额">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.totalNetWeight }}</span>
        </template>
      </el-table-column>
    </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  currentInfo: {
    type: Array,
    default: () => []
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  enclosureCategory: {
    type: [String, Number],
    default: undefined
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractChange',
  paginate: true,
  extraHeight: 40
})
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
