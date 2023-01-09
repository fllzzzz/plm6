<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.basicClass"
        :options="classificationEnum.ENUM"
        :unshowVal="[classificationEnum.GAS.V, classificationEnum.STRUC_MANUFACTURED.V, classificationEnum.ENCL_MANUFACTURED.V]"
        type="enum"
        size="small"
        class="filter-item"
        @change="handleChange"
      />
      <el-date-picker
        v-model="query.times"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        :disabled-date="disabledDate"
        :clearable="false"
        style="width:240px"
        @change="handleChange"
      />
      <template v-if="query.basicClass & STEEL_ENUM">
        <el-tooltip content="材质" :show-after="100" placement="top">
          <span>
            <common-select
              v-model="query.material"
              :options="options.list"
              :data-structure="{ key: 'label', label: 'value', value: 'label' }"
              class="filter-item"
              clearable
              filterable
              type="other"
              size="small"
              placeholder="可选择材质"
              @change="crud.toQuery"
            />
          </span>
        </el-tooltip>
        <el-tooltip v-if="query.basicClass === classificationEnum.SECTION_STEEL.V" content="科目" :show-after="100" placement="top">
          <span>
            <common-select
              v-model="query.classifyId"
              :options="options.KV?.[query.material]?.list || []"
              :data-structure="{ key: 'label', label: 'value', value: 'label' }"
              class="filter-item"
              clearable
              filterable
              type="other"
              size="small"
              placeholder="可选择科目"
              @change="crud.toQuery"
            />
          </span>
        </el-tooltip>
        <el-tooltip v-else content="厚度" :show-after="100" placement="top">
          <span>
            <common-select
              v-model="query.thickness"
              :options="options.KV?.[query.material]?.list || []"
              :data-structure="{ key: 'value', label: 'value', value: 'value' }"
              class="filter-item"
              clearable
              filterable
              type="other"
              size="small"
              placeholder="可选择厚度"
              @change="crud.toQuery"
            />
          </span>
        </el-tooltip>
      </template>
      <el-tooltip v-else content="科目" :show-after="100" placement="top">
          <span>
            <common-select
              v-model="query.classifyId"
              :options="options.list"
              :data-structure="{ key: 'label', label: 'value', value: 'label' }"
              class="filter-item"
              clearable
              filterable
              type="other"
              size="small"
              placeholder="可选择科目"
              @change="crud.toQuery"
            />
          </span>
        </el-tooltip>
      <rrOperation/>
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { ref, reactive } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS, STEEL_ENUM } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { toThousand, getDP } from '@data-type/number'
import { classificationEnum } from '@enum-ms/classification'
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])
const options = reactive({
  list: [],
  KV: {},
  update: true
})

const defaultQuery = {
  times: [moment().subtract(1, 'month').valueOf(), moment().valueOf()], // [开始时间，结束时间]
  basicClass: classificationEnum.STEEL_PLATE.V,
  material: undefined,
  thickness: undefined,
  classifyId: undefined
}

const { CRUD, crud, query } = regHeader(defaultQuery)

function disabledDate(time) {
  return time > new Date()
}

// 重新查询前
CRUD.HOOK.beforeToQuery = () => {
  if (!query.times) {
    query.times = undefined
  }
}

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  // 更新其余搜索条件数据
  if (options.update) {
    options.list = data.spec || []
    options.KV = {}
    options.list.forEach(v => {
      options.KV[v.label] = v
    })
    options.update = false
  }
  data.content = await setSpecInfoToList(data.content)
  data.content.forEach(v => {
    if (v.basicClass & STEEL_ENUM) {
      // 此页面钢材默认显示千克，保留3位
      v.accountingUnit = '千克'
      v.accountingPrecision = 3
    }
  })
  data.content = await numFmtByBasicClass(data.content, {
    toNum: true
  },
  {
    mete: ['inboundMete', 'outboundMete', 'inventoryMete']
  })
  data.content.forEach(v => {
    v.unitPriceExcludingVAT = toThousand(v.unitPriceExcludingVAT, getDP(v.unitPriceExcludingVAT))
  })
}

// 参数变化
function handleChange() {
  // 物料种类 或者 时间 改变后，清仓其余条件选择，并且接收 data.spec 数据
  query.material = undefined
  query.thickness = undefined
  query.classifyId = undefined
  options.update = true
  crud.toQuery()
}
</script>
