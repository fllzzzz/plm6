<template>
  <div class="head-container" style="display: flex; justify-content: flex-end">
    <span style="font-size: 14px">单位：件|吨</span>
  </div>
  <div class="app-container" :style="`height: ${maxHeight - 40}px; overflow-y: auto`">
    <div :style="`display: flex; flex-direction: column; `">
      <div class="item-side" v-for="item in processData" :key="item">
        <div v-if="item.productType === bridgeComponentTypeEnum .MACHINE_PART.V">
          <span>{{ item.name }}</span>
          <el-divider style="margin: 0 0 10px" />
          <div class="process-detail" style="display: flex; flex-wrap: wrap">
            <div
              class="show-detail"
              v-for="m in item?.arr"
              :key="m"
              style="background-color: #e6a23c; margin: 0 15px 10px 0"
              @click="layOffDetail(m)"
            >
              <span class="process-way">{{ m.process?.name }}</span>
              <el-divider style="margin: 0" />
              <span class="process-data">{{ m.quantity }}/{{ convertUnits(m.mete, 'kg', 't', DP.COM_WT__T) }}</span>
            </div>
          </div>
        </div>
      </div>
      <div class="item-side" v-for="item in processData" :key="item">
        <div v-if="item.productType === bridgeComponentTypeEnum .CELL.V">
          <span>{{ item.name }}</span>
          <el-divider style="margin: 0 0 10px" />
          <div class="process-detail" style="display: flex; flex-wrap: wrap">
            <div
              class="show-detail"
              v-for="m in item?.arr"
              :key="m"
              style="background-color: #409eff; margin: 0 15px 10px 0"
              @click="layOffDetail(m)"
            >
              <span class="process-way">{{ m.process?.name }}</span>
              <el-divider style="margin: 0" />
              <span class="process-data">{{ m.quantity }}/{{ convertUnits(m.mete, 'kg', 't', DP.COM_WT__T) }}</span>
            </div>
          </div>
        </div>
      </div>
      <div class="item-side" v-for="item in processData" :key="item">
        <div v-if="item.productType === bridgeComponentTypeEnum .BOX.V">
          <span>{{ item.name }}</span>
          <el-divider style="margin: 0 0 10px" />
          <div class="process-detail" style="display: flex; flex-wrap: wrap">
            <div
              class="show-detail"
              v-for="m in item?.arr"
              :key="m"
              style="background-color: #67c23a; margin: 0 15px 10px 0"
              @click="layOffDetail(m)"
            >
              <span class="process-way">{{ m.process?.name }}</span>
              <el-divider style="margin: 0" />
              <span class="process-data">{{ m.quantity }}/{{ convertUnits(m.mete, 'kg', 't', DP.COM_WT__T) }}</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import useMaxHeight from '@compos/use-max-height'
import { ref, defineEmits, defineProps, watch } from 'vue'
import { getAllProcess } from '@/api/bridge/bridge-task-tracking/process-sluggish.js'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

const processData = ref([])
const emit = defineEmits(['change'])
const props = defineProps({
  workShopId: {
    type: Number
  }
})

watch(
  () => props.workShopId,
  (val) => {
    allProcessDetail()
  }
)

// 获取车间下的所有工序
async function allProcessDetail() {
  processData.value = []
  try {
    const data = await getAllProcess({ workShopId: props.workShopId })
    data.map((v) => {
      v.uuid = v.productType + '_' + v.productionLine?.id
      if (processData.value.findIndex((k) => k.uuid === v.uuid) > -1) {
        const val = processData.value.find((k) => k.uuid === v.uuid)
        val.arr.push(v)
      } else {
        processData.value.push({
          uuid: v.uuid,
          productType: v.productType,
          name: bridgeComponentTypeEnum .VL[v.productType] + '>' + v.productionLine?.name,
          arr: [v]
        })
      }
    })
    console.log(processData.value, 'data')
  } catch (e) {
    console.log('获取车间的所有工序失败', e)
  }
}

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
function layOffDetail(val) {
  emit('change', val)
}
</script>

<style lang="scss" scoped>
.app-container {
  padding: 0 20px 20px 0;
}
.show-detail {
  width: 29%;
  display: flex;
  flex-direction: column;
  justify-content: space-evenly;
  align-items: center;
  color: #fff;
  cursor: pointer;
  font-size: 14px;
  margin-bottom: 10px;
  .process-way {
    padding: 10px 0;
  }
  .process-data {
    padding: 10px 0;
  }
}
</style>

