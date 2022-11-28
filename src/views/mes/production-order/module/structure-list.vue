<template>
  <div>
    <common-radio-button
      v-model="productType"
      :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.ASSEMBLE, componentTypeEnum.MACHINE_PART]"
      type="enum"
      style="margin-bottom: 10px"
      @change="fetchDetail"
    />
    <el-row v-loading="tableLoading" :gutter="30" class="panel-group">
      <el-col :span="8" class="card-panel-col">
        <Panel name="构件总数" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.artifactQuantity" is-array />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <Panel name="部件总数" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.assembleQuantity" is-array />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <Panel name="零件总数" text-color="#626262" num-color="#1890ff" :num-arr="totalAmount.machinePartQuantity" is-array />
      </el-col>
    </el-row>
    <div style="display: flex; margin-top: 15px">
      <div style="width: 45%; padding-right: 10px" class="tree-list">
        <common-table
          :data="list"
          v-loading="tableLoading"
          @current-change="handleCurrentChange"
          :data-format="dataFormat"
          highlight-current-row
          :max-height="maxHeight - 70"
        >
          <el-table-column
            key="classificationName"
            prop="classificationName"
            :label="`${componentTypeEnum.VL[productType]}分类`"
            align="center"
            :show-overflow-tooltip="true"
          >
            <template v-slot="scope">
              <span v-if="scope.row.classificationId === null" style="color: red">未知类型</span>
              <span v-else>{{ scope.row.classificationName }}</span>
            </template>
          </el-table-column>
          <el-table-column
            key="artifactType"
            prop="artifactType"
            label="构件类型"
            align="center"
            v-if="productType === componentTypeEnum.ARTIFACT.V"
            :show-overflow-tooltip="true"
          />
          <el-table-column key="material" prop="material" label="材质" align="center" :show-overflow-tooltip="true" />
          <el-table-column key="quantity" prop="quantity" label="数量" align="center" :show-overflow-tooltip="true" />
          <el-table-column key="totalNetWeight" prop="totalNetWeight" label="重量（kg）" align="center" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <span v-if="scope.row.totalNetWeight">{{ toThousand(scope.row.totalNetWeight, DP.COM_WT__KG) }}</span>
              <span v-else>-</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div style="border-right: 1px solid #ededed; height: calc(100vh - 260px)"></div>
      <div style="width: 54%; padding-left: 10px">
        <type-detail :currentRow="currentRow" v-if="isNotBlank(currentRow)" :productType="productType" />
        <div class="my-code" v-else>*点击左表操作查看明细</div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { artifactTreeData, artifactTreeSummary } from '@/api/mes/production-order-manage/production-order'

import useMaxHeight from '@compos/use-max-height'
import { isNotBlank } from '@data-type/index'
import { componentTypeEnum, artifactTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'

import typeDetail from './type-detail'
import Panel from './components/Panel'
// import checkPermission from '@/utils/system/check-permission'

const props = defineProps({
  currentId: {
    type: [String, Number],
    default: undefined
  },
  drawerVisible: {
    type: Boolean,
    require: true
  }
})

// const submitLoading = ref(false)
const list = ref([])
const tableLoading = ref(false)
const productType = ref(componentTypeEnum.ARTIFACT.V)
const currentRow = ref({})
const totalAmount = ref({})

const { maxHeight } = useMaxHeight({ extraBox: '.tag-div', wrapperBox: ['.app-container', '.tree-list'] })

const dataFormat = ref([
  ['artifactType', ['parse-enum', artifactTypeEnum]],
  ['auditReceiptTime', 'parse-time']
])

watch(
  () => props.drawerVisible,
  (val) => {
    if (val) {
      productType.value = componentTypeEnum.ARTIFACT.V
      currentRow.value = {}
      fetchDetail()
      getSummary()
    }
  },
  { deep: true, immediate: true }
)

async function getSummary() {
  totalAmount.value = {}
  if (!props.currentId) {
    return
  }
  tableLoading.value = true
  try {
    const data = await artifactTreeSummary({ projectId: props.currentId, productType: productType.value })
    totalAmount.value = {
      artifactQuantity: [
        {
          quantity: data.artifactQuantity || 0,
          unit: '件',
          precision: 0
        },
        {
          quantity: data.artifactMete || 0,
          unit: 'kg',
          precision: 2
        }
      ],
      assembleQuantity: [
        {
          quantity: data.assembleQuantity,
          unit: '件',
          precision: 0
        },
        {
          quantity: data.assembleMete,
          unit: 'kg',
          precision: 2
        }
      ],
      machinePartQuantity: [
        {
          quantity: data.machinePartQuantity,
          unit: '件',
          precision: 0
        },
        {
          quantity: data.machinePartMete,
          unit: 'kg',
          precision: 2
        }
      ]
    }
  } catch (error) {
    console.log('获取构零件清单汇总失败', error)
  }
}

async function fetchDetail() {
  list.value = []
  if (!props.currentId) {
    return
  }
  tableLoading.value = true
  try {
    const data = await artifactTreeData({ projectId: props.currentId, type: productType.value })
    data.map((v) => {
      v.projectId = props.currentId
    })
    list.value = data || []
    tableLoading.value = false
  } catch (error) {
    console.log('获取构零件清单失败', error)
    tableLoading.value = false
  }
}

function handleCurrentChange(val) {
  currentRow.value = val
}
</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom: 10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align: left;
        margin-top: 2px;
      }
      .card-panel-num {
        display: block;
        font-size: 20px;
        text-align: right;
      }
    }
  }
}
</style>
