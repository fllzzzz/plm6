<template>
  <common-dialog
    customClass="pad-block-scheduling-preview-dlg"
    title="标准零件排产预览"
    v-model="dialogVisible"
    width="1500px"
    :before-close="handleClose"
  >
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div>
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
      <el-badge :value="padBlockData.length" class="item">
        <common-button type="primary" size="mini" class="filter-item" @click="padBlockClick"> 标准零件列表</common-button>
      </el-badge>
    </div>
    <common-table :data="padBlockList" return-source-data :show-empty-symbol="false" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="left">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="left">
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度\n(mm)`" min-width="80px" align="left">
        <template #default="{ row }">
          <span>{{ row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="thick"
        prop="thick"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`厚度\n(mm)`"
        align="left"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ scope.row.thick ? scope.row.thick : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.material ? scope.row.material : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        key="netWeight"
        prop="netWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单净重\n(kg)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.netWeight ? scope.row.netWeight : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        key="grossWeight"
        prop="grossWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单毛重\n(kg)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.grossWeight ? scope.row.grossWeight : '-' }}
        </template>
      </el-table-column>
      <!-- <el-table-column
        key="totalNetWeight"
        prop="totalNetWeight"
        :show-overflow-tooltip="true"
        :label="`总净重\n(kg)`"
        align="left"
        min-width="95px"
      >
        <template v-slot="scope">
          {{ scope.row.totalNetWeight ? scope.row.totalNetWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        key="totalGrossWeight"
        prop="totalGrossWeight"
        :show-overflow-tooltip="true"
        :label="`总毛重\n(kg)`"
        align="left"
        min-width="95px"
      >
        <template v-slot="scope">
          {{ scope.row.totalGrossWeight ? scope.row.totalGrossWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column> -->
      <el-table-column key="surfaceArea" prop="surfaceArea" sortable="custom" :label="`单面积\n(㎡)`" align="left" min-width="80px">
        <template v-slot="scope">
          {{ scope.row.surfaceArea ? scope.row.surfaceArea : '-' }}
        </template>
      </el-table-column>
      <el-table-column key="picturePath" prop="picturePath" label="图片" align="left" min-width="80px">
        <template v-slot="scope">
          <div class="board-box">
            <el-image :src="scope.row.picturePath" @error="scope.row.imgLoad = false">
              <template #error>
                <div class="error-slot">
                  <span v-if="scope.row.picturePath">加载失败</span>
                  <span v-else>未导入DXF</span>
                </div>
              </template>
            </el-image>
          </div>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" label="已套料数量" align="left" width="120px">
        <template #default="{ row }">
          <span>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="usedQuantity" label="数量" align="left" width="120px" fixed="right">
        <template #default="{ row }">
          <el-input-number
            v-model="row.usedQuantity"
            :step="1"
            :min="1"
            :max="99999999"
            size="mini"
            style="width: 100%"
            controls-position="right"
          />
        </template>
      </el-table-column>
      <el-table-column label="操作" width="70" align="center" fixed="right">
        <template v-slot="scope">
          <common-button type="success" icon="el-icon-plus" size="mini" @click="add(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
  <pad-block v-model:visible="drawerVisible" :pad-block-data="props.padBlockData" />
</template>

<script setup>
import { getParallel, getParallelParams } from '@/api/config/system-config/parallel-config'
import { defineEmits, defineProps, ref } from 'vue'
// import { DP } from '@/settings/config'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import padBlock from './pad-block'

const emit = defineEmits(['update:visible', 'success', 'addBlock'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  padBlockData: {
    type: Array,
    default: () => []
  }
})
const padBlockList = ref([])
const thick = ref()
const material = ref()
const serialNumber = ref()
const specification = ref()
const thickList = ref([])
const materialList = ref([])
const drawerVisible = ref(false)
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
  // padBlockList.value = props.padBlockData
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

// 标准零件弹窗
async function fetchPadBlock() {
  console.log(material.value, thick.value)
  try {
    const { content } = await getParallel({
      serialNumber: serialNumber.value,
      specification: specification.value,
      material: material.value,
      thick: thick.value
    })
    content?.forEach((v) => {
      v.imgLoad = true
      v.usedQuantity = 1
    })
    padBlockList.value = content || []
  } catch (e) {
    console.log('获取标准零件列表信息失败', e)
  }
}

function add(row) {
  emit('addBlock', row)
}

// 标准零件列表
function padBlockClick() {
  drawerVisible.value = true
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
<style lang="scss" scoped>
.board-box {
  width: 80px;
  height: 80px;
  line-height: 80px;
  text-align: center;
  box-sizing: border-box;
  padding: 2px;
  border: 1px solid #dfe4ed;
  border-radius: 6px;
}
</style>
