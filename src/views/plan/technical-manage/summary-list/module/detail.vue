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
        :data="currentInfo"
        style="width: 100%"
        return-source-data
        :showEmptySymbol="false"
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="区域" width="140px">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column key="axis" prop="axis" :show-overflow-tooltip="true" label="轴线/标高" min-width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.axis }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="合计数量" min-width="150px">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="合计毛重(kg)" min-width="150px" v-if="!enclosureCategory">
        <template v-slot="scope">
          <span>{{ scope.row.totalGrossWeight?scope.row.totalGrossWeight.toFixed(DP.COM_WT__KG):'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalGrossWeight" prop="totalGrossWeight" :show-overflow-tooltip="true" label="合计净重(kg)" min-width="150px" v-if="!enclosureCategory">
        <template v-slot="scope">
          <span>{{ scope.row.totalNetWeight?scope.row.totalNetWeight.toFixed(DP.COM_WT__KG):'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalLength" prop="totalLength" :show-overflow-tooltip="true" label="合计量(m)" min-width="160px" v-if="enclosureCategory">
        <template v-slot="scope">
          <span>{{ scope.row.totalLength?scope.row.totalLength.toFixed(DP.COM_L__M):'-' }}</span>
        </template>
      </el-table-column>
    </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'
import useVisible from '@compos/use-visible'
import { DP } from '@/settings/config'

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

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
