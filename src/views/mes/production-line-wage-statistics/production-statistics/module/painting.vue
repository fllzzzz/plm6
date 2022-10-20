<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="构件"
    :wrapper-closable="false"
    size="80%"
  >
    <template #content>
      <div>工序：</div>
      <div style="display: flex">
        <el-tag >班组：</el-tag>
        <print-table
          v-permission="permission.printDetail"
          :api-key="apiKey"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </div>
      <common-table
        ref="tableRef"
        :data="tableData"
        :max-height="maxHeight"
        style="width: 100%"
        return-source-data
        :showEmptySymbol="false"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="项目">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}-{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="构件编号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="nuclear" prop="nuclear" :show-overflow-tooltip="true" label="核算单位">
          <template v-slot="scope">
            <span>{{ scope.row.nuclear }}</span>
          </template>
        </el-table-column>
        <el-table-column key="primer" prop="primer" :show-overflow-tooltip="true" label="底漆">
          <el-table-column key="area" prop="area" :show-overflow-tooltip="true" label="面积">
            <template v-slot="scope">
              <span>{{ scope.row.area? toThousand(scope.row.area): '/' }}</span>
            </template>
          </el-table-column>
          <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价">
            <template v-slot="scope">
              <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice) : '/' }}</span>
            </template>
          </el-table-column>
          <el-table-column key="totalPrice" prop="totalPrice" :show-overflow-tooltip="true" label="金额（元）">
            <template v-slot="scope">
              <span>{{ scope.row.totalPrice ? toThousand(scope.row.totalPrice) : '/' }}</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column key="intermediatePaint" prop="intermediatePaint" :show-overflow-tooltip="true" label="中间漆">
          <el-table-column key="area" prop="area" :show-overflow-tooltip="true" label="面积">
            <template v-slot="scope">
              <span>{{ scope.row.area? toThousand(scope.row.area): '/' }}</span>
            </template>
          </el-table-column>
          <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价">
            <template v-slot="scope">
              <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice) : '/' }}</span>
            </template>
          </el-table-column>
          <el-table-column key="totalPrice" prop="totalPrice" :show-overflow-tooltip="true" label="金额（元）">
            <template v-slot="scope">
              <span>{{ scope.row.totalPrice ? toThousand(scope.row.totalPrice) : '/' }}</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column key="Topcoat" prop="Topcoat" :show-overflow-tooltip="true" label="面漆">
          <el-table-column key="area" prop="area" :show-overflow-tooltip="true" label="面积">
            <template v-slot="scope">
              <span>{{ scope.row.area? toThousand(scope.row.area): '/' }}</span>
            </template>
          </el-table-column>
          <el-table-column key="unitPrice" prop="unitPrice" :show-overflow-tooltip="true" label="单价">
            <template v-slot="scope">
              <span>{{ scope.row.unitPrice ? toThousand(scope.row.unitPrice) : '/' }}</span>
            </template>
          </el-table-column>
          <el-table-column key="totalPrice" prop="totalPrice" :show-overflow-tooltip="true" label="金额（元）">
            <template v-slot="scope">
              <span>{{ scope.row.totalPrice ? toThousand(scope.row.totalPrice) : '/' }}</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column key="totalSum" prop="totalSum" :show-overflow-tooltip="true" label="合计">
          <template v-slot="scope">
            <span>{{ scope.row.totalSum ? toThousand(scope.row.totalSum) : '/' }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { occurLog } from '@/api/contract/contract-ledger'
import { toThousand } from '@data-type/number'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  projectId: {
    type: [String, Number],
    default: undefined
  }
})

const tableData = ref([])
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { maxHeight } = useMaxHeight({
  wrapperBox: '.occurAmountLog',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      getOccurLog()
    } else {
      tableData.value = []
    }
  },
  { deep: true, immediate: true }
)

async function getOccurLog() {
  try {
    const { content } = await occurLog({ projectId: props.projectId })
    tableData.value = content || []
  } catch (e) {
    console.log('获取变更金额记录', e)
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
