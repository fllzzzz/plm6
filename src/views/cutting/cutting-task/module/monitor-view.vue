<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="90%"
    :title="'1222'"
    :wrapper-closable="false"
    :before-close="Close"
  >
    <template #titleRight> </template>
    <template #content>
      <div class="big-box">
        <div class="Projection"></div>
        <div class="concent-box">
          <!-- <div class="text1"></div> -->
          <el-card style="margin-top: 5px" class="box-card text1">
            <template #header>
              <div class="card-header">
                <span style="text-align: center; color: black" class="orrSpan">机器信息</span>
              </div>
            </template>
            <div class="Content-box">
              <span class="orrSpan">
                机器名称：<span class="inSpan">{{ monitorMachine.machineName }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                机器类型：<span class="inSpan">{{ monitorMachine.machineType }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                车间：<span class="inSpan">{{ monitorMachine.workshopInf }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                位置：<span class="inSpan">{{ monitorMachine.position }}</span></span
              >
            </div>
            <div class="Content-box">
              <span class="orrSpan"> 当日用电量：<span class="inSpan">123</span> </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">当日消耗气体量：<span class="inSpan">123</span></span>
            </div>
          </el-card>
          <el-card style="margin-top: 5px" class="box-card text2">
            <template #header>
              <div class="card-header">
                <span style="text-align: center; color: black" class="orrSpan">当前钢板信息</span>
              </div>
            </template>
            <div class="Content-box">
              <span class="orrSpan">
                材质：
                <span class="inSpan" v-if="FileObj"> {{ FileObj.material }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                钢板编号： <span class="inSpan" v-if="FileObj">{{ FileObj.plateNo }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                物料种类： <span class="inSpan" v-if="FileObj">{{ FileObj.plateType }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                厚（㎜）： <span class="inSpan" v-if="FileObj">{{ FileObj.thick }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                宽（㎜）： <span class="inSpan" v-if="FileObj">{{ FileObj.width }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                长（㎜）： <span class="inSpan" v-if="FileObj">{{ FileObj.length }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                重量（㎏）： <span class="inSpan" v-if="FileObj">{{ FileObj.weight }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                品牌： <span class="inSpan" v-if="FileObj">{{ FileObj.brand }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                炉批号： <span class="inSpan" v-if="FileObj">{{ FileObj.furnaceNo }}</span>
              </span>
            </div>
          </el-card>
          <el-card style="margin-top: 5px" class="box-card text3">
            <template #header>
              <div class="card-header">
                <span style="text-align: center; color: black" class="orrSpan">实时切割信息</span>
              </div>
            </template>
            <div class="Content-box">
              <span class="orrSpan">
                文件名：
                <span class="inSpan" v-if="websocketData && websocketData.fileName">{{ websocketData.fileName }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                运行状态：
                <span class="inSpan" v-if="websocketData && websocketData.cutState">{{ websocketData.cutState }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                当前行号/孔号：
                <span class="inSpan" v-if="websocketData && websocketData.currentLineNum && websocketData.websocketData">
                  {{ websocketData.currentLineNum }} / {{ websocketData.currentHoleNum }}
                </span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                切割速度：<span class="inSpan" v-if="websocketData && websocketData.cutSpeed">{{ websocketData.cutSpeed }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">校正角度：<span class="inSpan">123</span></span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">手动速度：<span class="inSpan">123</span></span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                穿孔次数：
                <span class="inSpan" v-if="websocketData && websocketData.totalPierceCount">{{ websocketData.totalPierceCount }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                定长距离：
                <span class="inSpan" v-if="websocketData && websocketData.moveFixedDistanc">{{ websocketData.moveFixedDistanc }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                切割距离：
                <span class="inSpan" v-if="websocketData && websocketData.totalCutLength">{{ websocketData.totalCutLength }}</span>
              </span>
            </div>

            <div class="Content-box">
              <span class="orrSpan">
                割缝：<span class="inSpan" v-if="websocketData && websocketData.kerf">{{ websocketData.kerf }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                预热时间：
                <span class="inSpan" v-if="websocketData && websocketData.estimateCutTime">{{ websocketData.estimateCutTime }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                切割时间：
                <span class="inSpan" v-if="websocketData && websocketData.currentCutTime">{{ websocketData.currentCutTime }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">穿孔时间：<span class="inSpan">123</span> </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">运动延时：<span class="inSpan">123</span></span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">
                运行时间：<span class="inSpan" v-if="websocketData && websocketData.softRunTime">{{ websocketData.softRunTime }}</span>
              </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">切割高度：<span class="inSpan">123</span> </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">穿孔高度：<span class="inSpan">123</span> </span>
            </div>
            <div class="Content-box">
              <span class="orrSpan">版本号：<span class="inSpan">123</span></span>
            </div>
          </el-card>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { getPlantByFileName } from '@/api/cutting/machine'
import { defineProps, defineEmits, ref } from 'vue'

const drawerRef = ref()
const FileObj = ref()
const emit = defineEmits(['update:Change', 'closeMonitor'])
const props = defineProps({
  visible: { type: Boolean, default: false },
  monitorMachine: { type: Object, require: true },
  websocketData: { type: Object, require: true }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

async function showHook() {
  console.log('showHook!!')
  const data = await getPlantByFileName('P3 100000.NC')
  console.log(data)
  data.forEach((item, index) => {
    if (index === 0) {
      FileObj.value = item
      console.log(' FileObj.value', FileObj.value)
    }
  })
}
function Close() {
  handleClose()
  emit('closeMonitor')
  console.log('关闭触发!!')
}
</script>

<style lang="scss" scoped>
.el-card__header {
  text-align: center !important;
}
.big-box {
  width: 100%;
  height: 800px;

  .Projection {
    float: left;
    width: 65%;
    height: 100%;
    background-color: #fff;
  }
  .concent-box {
    .el-card__header {
      .card-header {
        text-align: center;
      }
    }

    float: left;
    width: 35%;
    height: 100%;

    .text1 {
      .Content-box {
        float: left;
        width: 50%;
        height: 30px;
        line-height: 30px;
        text-align: center;
      }
      width: 100%;
      height: 25%;
    }
    .text2 {
      width: 100%;
      height: 25%;
      .Content-box {
        float: left;
        width: 33%;
        height: 30px;
        line-height: 30px;
        text-align: center;
      }
    }
    .text3 {
      width: 100%;
      height: 50%;
      .Content-box {
        float: left;
        width: 33%;
        height: 30px;
        line-height: 30px;
        text-align: center;
      }
    }
  }
  .orrSpan {
    font-weight: 900;
    color: #909399;
    .inSpan {
      font-weight: 500;
      color: #5a5959;
      size: 18px;
    }
  }
}
</style>
