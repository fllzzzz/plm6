 <template>
  <div>
    <el-card :style="{ height: `${maxHeight}px` }">
      <template #header>
        <div class="card-header flex-css">
          <div class="flex-rbc">机器列表</div>
        </div>
      </template>

      <div class="card-body">
        <common-button class="filter-item" size="mini" type="success" @click="Issue(item, 0)">下 发</common-button>
        <common-button style="float: right" class="filter-item" size="mini" type="danger" @click="eliminate(item, 1)">清 除</common-button>
        <common-button style="float: right" class="filter-item" size="mini" type="primary" @click="changeTaskClick(item, 2)">
          转 产
        </common-button>
        <div class="card-div" :style="{ height: `${maxHeight - 120}px` }">
          <el-card v-for="item in machineData" :key="item.id" ref="card" class="box-card">
            <div class="title" style="width: 100%">
              <h5
                :class="item.online === 0 ? 'red' : item.online === 1 ? 'green' : 'orange'"
                class="inTitle"
                style="margin: 0px; float: left"
              >
                {{ MessageTypeEnum.VL[item.online] }}
              </h5>
              <el-checkbox class="inTitle" style="float: right; margin-top: -5px" @change="checkClick(item)" v-model="item.checkout" />
            </div>
            <div class="content" style="width: 100%; word-spacing: -1em; height: 100%">
              <div class="img-box" style="float: left"><img @click="imgClick(item.mac)" src="./machine.png" alt="" /></div>
              <div class="right-box" style="float: left">
                <!-- <div style="margin: 0px"><span>{{ item.machineName }}</span></div> -->
                <span>{{ item.machineName }}</span>
              </div>
              <div class="right-box" style="float: left">
                <h6 style="margin: 0px">类型：{{ item.machineType }}</h6>
              </div>
              <div class="right-box" style="float: left">
                <h6 style="margin: 0px">车间：{{ item.workshopInf }}</h6>
              </div>
              <div class="right-box" style="float: left">
                <h6 style="margin: 0px">位置：{{ item.position }}</h6>
              </div>
              <div class="right-box" style="width: 100%; float: left">
                <h6 style="margin: 0px; margin-left: 20px">
                  <span>分配量：</span>
                  <common-button @click="IssueClick(item.mac, 1)" style="font-weight: 700" type="text">
                    {{ item.assignNum }}/{{ item.assignWeight }}
                  </common-button>
                </h6>
              </div>
              <div class="right-box" style="width: 100%; float: left">
                <h6 style="width: 100%; margin: 0px; margin-left: 20px">
                  <span>下发量：</span>
                  <common-button @click="IssueClick(item.mac, 2)" style="font-weight: 700" type="text">
                    {{ item.sentNum }}/{{ item.sentWeight }}
                  </common-button>
                </h6>
              </div>
            </div>
            <div style="text-align: center" class="last-btn">
              <common-button style="text-align: center" size="mini" type="primary" icon="el-icon-view" @click="monitor(item)">
                监控
              </common-button>
            </div>
          </el-card>
        </div>
      </div>
    </el-card>
    <issue-view @updateChange="updateChange" :machineData="machineData" :IssueMac="IssueMac" v-model:visible="IssueVisible"></issue-view>
    <monitor-view
      :websocketData="websocketData"
      :monitorMachine="monitorMachine"
      @closeMonitor="closeMonitor"
      v-model:visible="monitorVisible"
    />
  </div>
</template>

<script setup>

import issueView from './config.vue'
import monitorView from './monitor-view.vue'
import { ElMessage } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import { defineProps, ref, defineEmits } from 'vue'
import { get, assign } from '@/api/cutting/machine'
import { MessageTypeEnum } from '@enum-ms/cutting'

const props = defineProps({
  list: {
    type: Array,
    required: true
  }
})

let ws = null
let mac = ref('')
const monitorMachine = ref()
const websocketData = ref()
const IssueVisible = ref(false)
const monitorVisible = ref(false)
const maxReconnect = 5 // 最大重连次数，若连接失败
const lockReconnect = false // 连接失败不进行重连
const emit = defineEmits(['query'])
const wsUri = 'ws://172.16.3.105:8084'// ws wss
// const wsUri = 'ws://172.16.3.105:8088/zs/websocket'// ws wss

const heartCheck = {
  timeout: 6 * 1000,
  timer: null,
  serverTimer: null,
  reset() {
    this.timer && clearTimeout(this.timer)
    this.serverTimer && clearTimeout(this.serverTimer)
  },
  start(ws) {
    this.reset()
    this.timer = setTimeout(() => {
      // console.log('发送心跳,后端收到后，返回一个心跳消息')
      // onmessage拿到返回的心跳就说明连接正常
      ws.send(JSON.stringify({ heart: 1 }))
      this.serverTimer = setTimeout(() => {
        // 如果超过一定时间还没响应(响应后触发重置)，说明后端断开了
        ws.close()
      }, this.timeout)
    }, this.timeout)
  }
}

// 监控ing..
function monitor(item) {
  monitorMachine.value = item
  console.log('monitorMachine.value', monitorMachine.value)
  mac = item.mac
  monitorVisible.value = true
  initWebSocket()
}

function initWebSocket() {
  try {
    if ('WebSocket' in window) {
      ws = new WebSocket(wsUri)
    } else {
      console.log('您的浏览器不支持websocket')
    }
    // ws = new WebSocket(wsUri)
    ws.onopen = webSocketOnOpen
    ws.onerror = websocketOnError
    ws.onmessage = websocketOnMessage
    ws.onclose = webSocketClose
  } catch (err) {
    console.log('err', err)
    reconnect()
  }
}
function reconnect() {
  console.log('尝试重连')
  if (lockReconnect || maxReconnect <= 0) {
    return
  }
  setTimeout(() => {
    // maxReconnect-- // 不做限制 连不上一直重连
    initWebSocket()
  }, 0 * 1000)
}

function webSocketOnOpen() {
  console.log('WebSocket连接成功')
  heartCheck.start(ws)
  // socket.send('发送数据')
  webSocketSend()
}

function websocketOnError(e) {
  console.log('WebSocket连接发生错误', e)
  reconnect()
}

function websocketOnMessage(e) {
  websocketData.value = JSON.parse(e.data)
  console.log('websocketData.value', websocketData.value)
  // const data = JSON.parse(e.data)
  // console.log('得到响应', data)
  // console.log('可以渲染网页数据...')
  // 消息获取成功，重置心跳
  heartCheck.start(ws)
}

function webSocketClose(e) {
  console.log('connection closed (' + e.code + ')')
  reconnect()
}

// 传输数据
function webSocketSend() {
  const data = { MachineInformation: mac }
  ws.send(JSON.stringify(data))
}
function destroyed() {
  console.log('ws', ws)
  ws.close()
}
function closeMonitor() {
  destroyed()
}
const machineData = ref([]) // 页面数据

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

const IssueMac = {
  mac: '',
  plateState: undefined,
  type: undefined
}

getAdd()

async function getAdd() {
  const { content } = await get()
  machineData.value = content
  machineData.value.forEach((item) => {
    item.checkout = false
  })
}

async function imgClick(mac) {
  if (props.list.length > 0) {
    try {
      const message = await assign({ mac: mac }, props.list)
      ElMessage({ message: message, type: 'success' })
      emit('query')
      getAdd()
    } catch (err) {
      console.log(err)
    }
  }
}

function updateChange() {
  emit('query')
  getAdd()
}

async function IssueClick(item, num, type) {
  try {
    init()
    IssueMac.mac = item
    IssueMac.plateState = num
    IssueMac.type = type
    IssueVisible.value = true
  } catch (err) {
    console.log(err)
  }
}

async function eliminate(item, type) {
  const judge = machineData.value.some((item) => item.checkout === true)
  if (judge) {
    try {
      IssueClick(mac.value, 1, type)
      emit('query')
    } catch (err) {
      console.log(err)
    }
  } else {
    ElMessage({ message: '请选择机器进行任务清除', type: 'error' })
  }
}

async function Issue(item, type) {
  const judge = machineData.value.some((item) => item.checkout === true)
  if (judge) {
    try {
      IssueClick(mac.value, 1, type)
      emit('query')
    } catch (err) {
      console.log(err)
    }
  } else {
    ElMessage({ message: '请选择机器进行任务下发', type: 'error' })
  }
}

async function changeTaskClick(item, type) {
  const judge = machineData.value.some(item => item.checkout === true)
  if (judge) {
    try {
      IssueClick(mac.value, 1, type)
      emit('query')
    } catch (err) {
      console.log(err)
    }
  } else {
    ElMessage({ message: '请选择机器进行任务转产', type: 'error' })
  }
}

function checkClick(item) {
  mac.value = item.mac
  machineData.value.forEach((arr) => {
    if (arr.id !== item.id) {
      arr.checkout = false
    }
  })
}

function init() {
  IssueMac.mac = ''
  IssueMac.plateState = undefined
}

</script>

<style lang="scss" scoped>
.box-card {
  .red {
    color: red;
  }
  .green {
    color: #85ce61;
  }
  .orange {
    color: orange;
  }
  float: left;
  margin: 10px;
  height: 250px;
  width: 195px;
  .el-card__body {
    width: 100%;
    height: 100%;
    padding: 15px;
  }
  .title {
    height: 32px;
    padding: 0;
    margin: 0;
  }
  .content {
    overflow: hidden;
    height: 150px;
  }
  .img-box {
    width: 50px;
    height: 85px;
    img {
      width: 100%;
      height: 100%;
    }
  }
  .right-box {
    width: 93px;
    white-space: nowrap;
    text-overflow: ellipsis;
    overflow: hidden;
    word-break: break-all;
    height: 22px;
    line-height: 25px;
    overflow: hidden;
    margin-left: 10px;
  }
  .last-btn {
    margin-top: 20px;
  }
}
.card-div {
  margin-top: 45px;
  padding-bottom: 40px;
  overflow-y: auto;
}
</style>

