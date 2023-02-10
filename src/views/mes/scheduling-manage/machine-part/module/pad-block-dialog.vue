<template>
  <common-dialog
    customClass="pad-block-scheduling-preview-dlg"
    title="垫块排产预览"
    v-model="dialogVisible"
    width="1500px"
    :before-close="handleClose"
  >
    <div class="head-container">
      <common-select
        v-model="material"
        :options="materialList"
        :dataStructure="{ key: 'value', label: 'value', value: 'value' }"
        clearable
        filterable
        allow-create
        type="other"
        class="filter-item"
        placeholder="请选择材质"
        style="width: 160px"
        @change="fetchPadBlock"
      />
      <common-select
        v-model="thick"
        :options="thickList"
        :dataStructure="{ key: 'value', label: 'value', value: 'value' }"
        clearable
        filterable
        allow-create
        type="other"
        class="filter-item"
        placeholder="请选择厚度"
        style="width: 160px"
        @change="fetchPadBlock"
      />
      <el-input
        v-model="serialNumber"
        placeholder="输入编号搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="fetchPadBlock"
      />
      <el-input
        v-model="specification"
        placeholder="输入规格搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="fetchPadBlock"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </div>
    <common-table :data="padBlockList" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度(mm)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" :label="`单净重(kg)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" label="数量" align="center" width="120px" fixed="right">
        <template #default="{ row }">
          <template v-if="!row.boolOneCode">
            <el-input-number
              v-model="row.quantity"
              :step="1"
              :min="1"
              :max="row.quantity"
              size="mini"
              style="width: 100%"
              controls-position="right"
            />
          </template>
          <template v-else>
            <span>{{ row.quantity }}</span>
          </template>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="70" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            type="success"
            icon="el-icon-plus"
            size="mini"
            @click="add(scope.row)"
          />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { get, getParallelParams } from '@/api/config/system-config/parallel-config'
import { defineEmits, defineProps, ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'success', 'add'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})
const padBlockList = ref([])
const dataFormat = ref([['project', 'parse-project']])
const thick = ref()
const material = ref()
const serialNumber = ref()
const specification = ref()
const thickList = ref([])
const materialList = ref([])

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.machine-part-scheduling-preview-dlg',
    extraBox: ['.el-dialog__header', 'head-container'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

function showHook() {
  fetchPadBlock()
}

// 材质、厚度下拉选择
getParams()

async function getParams() {
  materialList.value = []
  thickList.value = []
  try {
    const data = await getParallelParams()
    for (let i = 0; i < data?.material.length; i++) {
      materialList.value.push({ value: data.material[i] })
    }
    for (let i = 0; i < data?.thick.length; i++) {
      thickList.value.push({ value: data.thick[i] })
    }
  } catch (e) {
    console.log('获取厚度材质筛选', e)
  }
}

// 垫块弹窗
async function fetchPadBlock() {
  try {
    const { content } = await get({
      serialNumber: serialNumber.value,
      specification: specification.value,
      material: material.value,
      thick: thick.value
    })
    padBlockList.value = content || []
  } catch (e) {
    console.log('获取生产组的信息失败', e)
  }
}

function add(row) {
  emit('add', row)
}

// 重置
function resetQuery() {
  serialNumber.value = undefined
  specification.value = undefined
  material.value = undefined
  thick.value = undefined
  fetchPadBlock()
}

// 搜索
function searchQuery() {
  fetchPadBlock()
}
</script>
