<template>
  <div class="match-table">
    <div class="filter-container">
      <div class="filter-left-box">
        <span class="table-title">公共库 · 物料匹配列表</span>
      </div>
      <div class="filter-right-box">
        <el-input
          v-model.trim="queryFilter.factoryName"
          clearable
          size="mini"
          placeholder="工厂名称"
          class="filter-item"
          style="width: 220px"
        />
        <el-checkbox class="filter-item" v-model="queryFilter.boolNotInInventoryList" label="只显示未加入库存利用清单" size="mini" border />
      </div>
    </div>
    <common-table
      ref="tableRef"
      v-loading="loading"
      :data="filterList"
      :default-expand-all="false"
      :height="props.height"
      :empty-text="emptyText"
      highlight-current-row
      row-key="id"
    >
      <!-- 基础信息 -->
      <material-base-info-columns
        :show-is-whole="true"
        :basic-class="props.matchInfo.basicClass"
        show-frozen-tip
        frozen-viewable
        spec-merge
        fixed="left"
        @refresh="fetchList"
      />
      <!-- 次要信息 -->
      <material-secondary-info-columns :basic-class="props.matchInfo.basicClass" :show-batch-no="false" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-operate-quantity-columns outbound-type-mode equal-disabled />
      <!-- 工厂/仓库 -->
      <warehouse-info-columns />
      <!-- 操作 -->
      <el-table-column label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <span v-if="inventoryExitIdMap.get(row.id)" style="color: #1890ff">已加入清单</span>
          <common-button v-else icon="el-icon-plus" type="success" size="mini" class="icon-button" @click="handleAdd(row)" />
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { getMatchSteelPlateList, getMatchSectionSteelList } from '@/api/plan/material-preparation/material-match'

import { ref, computed, defineProps, defineEmits, watch, inject } from 'vue'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { pinyinForField, pinyinFuzzyMatching } from '@/utils/pinyin'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'

const emit = defineEmits(['add'])

const props = defineProps({
  height: {
    type: Number,
    default: 250
  },
  matchInfo: {
    type: Object,
    default: () => ({})
  }
})

const crud = inject('crud')
const inventoryExitIdMap = ref()
const interfaceKey = ref(0) // 接口请求key，避免接口异步回调覆盖
watch(
  () => crud.props.inventoryExitIdMap,
  (map) => {
    inventoryExitIdMap.value = map
  },
  { immediate: true }
)

// 匹配加载
const loading = ref(false)

// 公共库材料匹配列表
const list = ref([])

// 拼音
const pinyinFields = ['factory.name', 'factory.shortName']

// 查询过滤
const queryFilter = ref({
  factoryName: undefined,
  boolNotInInventoryList: false
})
// 过滤后的列表
const filterList = computed(() => {
  return list.value.filter((row) => {
    let meets = true
    const factoryName = queryFilter.value.factoryName
    if (factoryName) {
      meets = pinyinFuzzyMatching(row, factoryName, pinyinFields)
    }
    if (meets && queryFilter.value.boolNotInInventoryList) {
      meets = !inventoryExitIdMap.value.get(row.id)
    }
    return meets
  })
})

const emptyText = computed(() => {
  return isBlank(props.matchInfo) || loading.value ? '可选择左侧清单数据，匹配物料系统公共库中对应的的材料' : '未匹配到物料'
})

watch(
  () => props.matchInfo,
  () => {
    fetchList()
  },
  { immediate: true }
)

function init() {
  list.value = []
}

// 加载列表
async function fetchList() {
  const key = ++interfaceKey.value
  init()
  const info = props.matchInfo
  if (isBlank(info)) return
  try {
    loading.value = true
    let matchList = []
    switch (info.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
        matchList = await matchListForSteelPlate(info)
        break
      case rawMatClsEnum.SECTION_STEEL.V:
        matchList = await matchListForSectionSteel(info)
        break
      default:
        throw Error('物料主分类错误')
    }
    if (key !== interfaceKey.value) throw Error('repeated calls')
    // 格式装换
    await setSpecInfoToList(matchList)
    await numFmtByBasicClass(matchList, {
      toNum: true
    })
    await calcTheoryWeight(matchList)

    matchList.forEach((row) => {
      row.operableQuantity = row.quantity - (row.frozenQuantity || 0)
      row.operableMete = row.mete - (row.frozenMete || 0)
      if (row.outboundUnitType === measureTypeEnum.MEASURE.V) {
        // 实际在出库中使用的数量
        row.corQuantity = row.quantity // 数量
        row.corFrozenQuantity = row.frozenQuantity // 冻结数量
        row.corOperableQuantity = row.operableQuantity // 可操作数量
      } else {
        // 核算量
        row.corQuantity = row.mete
        row.corFrozenQuantity = row.frozenMete
        row.corOperableQuantity = row.operableMete
      }

      row.operableNumber = row.corOperableQuantity
    })

    // 拼音转换
    list.value = pinyinForField(matchList, pinyinFields)
    loading.value = false
  } catch (error) {
    console.error('匹配物料', error)
    if (String(error).indexOf('repeated calls') === -1) {
      loading.value = false
    }
  }
}

// // 处理添加
function handleAdd(row) {
  emit('add', row, props.matchInfo)
}

// 匹配钢材列表
async function matchListForSteelPlate(info) {
  const query = {
    steelClassifyConfId: info.steelClassifyConfId, // 钢材分类配置id
    material: info.material, // 材质
    specification: info.specification // 厚度
  }
  const { content = [] } = await getMatchSteelPlateList(query)
  return content
}

async function matchListForSectionSteel(info) {
  const query = {
    steelClassifyConfId: info.steelClassifyConfId, // 钢材分类配置id
    material: info.material, // 材质
    specification: info.specification // 规格
  }
  const { content = [] } = await getMatchSectionSteelList(query)
  return content
}
</script>

<style lang="scss" scoped>
.match-table {
  width: inherit;
  // position: relative;
}

.table-title {
  background: dodgerblue;
  color: white;
  font-size: 14px;
  margin-bottom: 10px;
  margin-right: 8px;
  display: inline-block;
  padding: 3px 10px;
}
</style>