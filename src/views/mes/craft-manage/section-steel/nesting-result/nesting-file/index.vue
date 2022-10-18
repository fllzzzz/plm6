<template>
  <!-- 套料文件弹窗 -->
  <common-drawer :before-close="handleClose" size="70%" modal append-to-body v-model:visible="nestingFileVisible">
    <template #title>
      <common-radio-button
        style="margin-right: 8px"
        class="filter-item"
        v-model="nestingFileType"
        :options="nestingFileTypeEnum.ENUM"
        type="enum"
        size="small"
      />
       <common-button size="small" @click="handleClose">关闭</common-button>
    </template>
    <template #content>
      <common-table
        v-loading="innerLoading"
        ref="tableDrawerRef"
        :data="props.detailData"
        :max-height="400"
        style="width: 100%"
        row-key="id"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomerId" prop="monomerId" :show-overflow-tooltip="true" label="单体" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.mete }}</span>
          </template>
        </el-table-column>
        <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="artifactNumber" prop="artifactNumber" :show-overflow-tooltip="true" label="关联构件编号" align="center">
          <template #default="{ row }">
            <span>{{ row.artifactNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="部件编号"
          min-width="60"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" min-width="60" align="center">
          <template #default="{ row }">
            <span>{{ row.netWeight }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script  setup>
import useVisible from '@compos/use-visible'
// import useMaxHeight from '@compos/use-max-height'
import { ref, defineProps, defineEmits } from 'vue'
import { nestingFileTypeEnum } from '@enum-ms/mes'

const nestingFileType = ref(nestingFileTypeEnum.NESTING_FILE.V)
const emit = defineEmits(['success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})
const { visible: nestingFileVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
</script>

<style>
</style>
